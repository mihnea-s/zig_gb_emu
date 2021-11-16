const std = @import("std");

const memory = @import("memory.zig");

pub usingnamespace @import("instructions.zig");

pub const CPU = struct {
    const Self = @This();

    const PseudoRNG = std.rand.DefaultPrng;
    const InstructionMap = std.AutoHashMap(u8, Instruction);

    /// Speed of GameBoy CPU in number of clock cycles
    /// (also called t-cycles) per second.
    ///
    /// `1 Mhz = 10 ** 6 Clocks per second`
    pub const Speed = 10 ** 6;

    const IxArgument = enum {
        reg_a,
        reg_f,
        reg_af,

        reg_b,
        reg_c,
        reg_bc,

        reg_d,
        reg_e,
        reg_de,

        reg_h,
        reg_l,
        reg_hl,

        val_x,
        val_xi,
        val_xx,
        reg_sp,

        ptr_de,
        ptr_bc,
        ptr_hl,
        ptr_xx,
    };

    /// Wether the jump instruction argument
    /// is an *absolute* position in memory or
    /// an offset *relative* to the current position.
    const IxJumpMode = enum {
        relative,
        absolute,
    };

    ram: *memory.Memory,
    allocator: *std.mem.Allocator,

    registers: Registers,
    pseudorng: PseudoRNG,

    pub fn init(ram: *memory.Memory, allocator: *std.mem.Allocator) !Self {
        return Self{
            .ram = ram,
            .allocator = allocator,

            .registers = Registers{},
            .pseudorng = PseudoRNG.init(@intCast(u64, std.time.milliTimestamp())),
        };
    }

    pub fn deinit(self: *Self) void {}

    fn get_flag(self: *Self, comptime flag: []const u8) bool {
        return @field(self.registers.af.ind.f, flag) == 1;
    }

    fn set_flag(self: *Self, comptime flag: []const u8, cond: bool) void {
        @field(self.registers.af.ind.f, flag) = if (cond) 1 else 0;
    }

    fn is_argument_16bit(comptime target: IxArgument) bool {
        return switch (target) {
            .reg_af,
            .reg_bc,
            .reg_de,
            .reg_hl,
            .val_xx,
            .reg_sp,
            => true,
            else => false,
        };
    }

    fn get_8bit_argument_address(self: *Self, comptime target: IxArgument) *u8 {
        return switch (target) {
            .reg_a => &self.registers.af.ind.a,
            .reg_f => &self.registers.af.ind.f,
            .reg_b => &self.registers.bc.ind.b,
            .reg_c => &self.registers.bc.ind.c,
            .reg_d => &self.registers.de.ind.d,
            .reg_e => &self.registers.de.ind.e,
            .reg_h => &self.registers.hl.ind.h,
            .reg_l => &self.registers.hl.ind.l,
            .ptr_bc => self.ram.read_ptr(u8, self.registers.bc.all),
            .ptr_de => self.ram.read_ptr(u8, self.registers.de.all),
            .ptr_hl => self.ram.read_ptr(u8, self.registers.hl.all),
            .ptr_xx => self.ram.read_ptr(u8, self.ram.read(u16, self.registers.pc + 1)),
            else => @compileError("invalid 8bit target"),
        };
    }

    fn get_8bit_argument_value(self: *Self, comptime target: IxArgument) u8 {
        return switch (target) {
            .val_x => self.ram.read(u8, self.registers.pc + 1),
            else => self.get_8bit_argument_address(target).*,
        };
    }

    fn get_16bit_argument_address(self: *Self, comptime target: IxArgument) *align(1) u16 {
        return switch (target) {
            .reg_af => &self.registers.af.all,
            .reg_bc => &self.registers.bc.all,
            .reg_de => &self.registers.de.all,
            .reg_hl => &self.registers.hl.all,
            .reg_sp => &self.registers.sp,
            .ptr_bc => self.ram.read_ptr(u16, self.registers.bc.all),
            .ptr_de => self.ram.read_ptr(u16, self.registers.de.all),
            .ptr_hl => self.ram.read_ptr(u16, self.registers.hl.all),
            .ptr_xx => self.ram.read_ptr(u16, self.ram.read(u16, self.registers.pc + 1)),
            else => @compileError("invalid 16bit target"),
        };
    }

    fn get_16bit_argument_value(self: *Self, comptime target: IxArgument) u16 {
        return switch (target) {
            .val_xx => self.ram.read(u16, self.registers.pc + 1),
            else => self.get_16bit_argument_address(target).*,
        };
    }

    fn increment(self: *Self, comptime arg: IxArgument) void {
        if (comptime is_argument_16bit(arg)) {
            const reg = self.get_16bit_argument_address(arg);
            const val = reg.*;

            _ = @addWithOverflow(u16, val, 1, reg);
        } else {
            const reg = self.get_8bit_argument_address(arg);
            const val = reg.*;

            self.set_flag("zero", val == 0xFF);
            self.set_flag("nsub", false);
            self.set_flag("half", (val & 0xF) == 0xF);

            _ = @addWithOverflow(u8, val, 1, reg);
        }
    }

    fn decrement(self: *Self, comptime arg: IxArgument) void {
        if (comptime is_argument_16bit(arg)) {
            const reg = self.get_16bit_argument_address(arg);
            const val = reg.*;

            _ = @subWithOverflow(u16, val, 1, reg);
        } else {
            const reg = self.get_8bit_argument_address(arg);
            const val = reg.*;

            self.set_flag("zero", val == 0x01);
            self.set_flag("nsub", true);
            self.set_flag("half", (val & 0xF) == 0x0);

            _ = @subWithOverflow(u8, val, 1, reg);
        }
    }

    fn load(self: *Self, comptime to: IxArgument, comptime from: IxArgument) void {
        if (comptime is_argument_16bit(from)) {
            self.get_16bit_argument_address(to).* = self.get_16bit_argument_value(from);
        } else {
            self.get_8bit_argument_address(to).* = self.get_8bit_argument_value(from);
        }
    }

    fn load_decrement(
        self: *Self,
        comptime to: IxArgument,
        comptime from: IxArgument,
        comptime incr: IxArgument,
    ) void {
        self.load(to, from);
        self.decrement(incr);
    }

    fn load_increment(
        self: *Self,
        comptime to: IxArgument,
        comptime from: IxArgument,
        comptime decr: IxArgument,
    ) void {
        self.load(to, from);
        self.increment(decr);
    }

    const IxJumpFlag = enum {
        always,
        zero,
        not_zero,
        carry,
        not_carry,
    };

    fn jump(
        self: *Self,
        comptime flag: IxJumpFlag,
        comptime jump_mode: IxJumpMode,
        comptime target: IxArgument,
    ) void {
        const condition = switch (flag) {
            .always => true,
            .zero => self.get_flag("zero"),
            .not_zero => !self.get_flag("zero"),
            .carry => self.get_flag("cary"),
            .not_carry => !self.get_flag("cary"),
        };

        if (!condition) {
            return;
        }

        const pc = &self.registers.pc;

        if (jump_mode == .relative and target == .val_xi) {
            const xi = self.ram.read(i8, self.registers.pc + 1);
            pc.* = @intCast(u16, @intCast(i32, pc.*) + xi);
            return;
        }

        switch (jump_mode) {
            .relative => pc.* += self.get_8bit_argument_value(target),
            .absolute => pc.* = self.get_16bit_argument_value(target),
        }
    }

    fn complement(self: *Self, comptime arg: IxArgument) void {
        self.get_8bit_argument_address(arg).* = ~self.get_8bit_argument_value(arg);
    }

    fn complement_carry_flag(self: *Self) void {
        self.set_flag("nsub", false);
        self.set_flag("half", false);
        self.set_flag("cary", !self.get_flag("cary"));
    }

    fn set_carry_flag(self: *Self) void {
        self.set_flag("nsub", false);
        self.set_flag("half", false);
        self.set_flag("cary", true);
    }

    fn interrupts(self: *Self, comptime enabled: bool) void {
        self.registers.ie = enabled;
    }

    pub fn step_instruction(self: *Self) usize {
        const ix = self.ram.read(u8, self.registers.pc);

        if (ix == 0xCB) {
            self.registers.pc += 1;
            return self.step_prefixed_instruction();
        }

        const details = instructions_details(ix) orelse std.debug.panic(
            "Unknown '0x{X:0>2}' @ ${X:0>4}\n",
            .{ ix, self.registers.pc },
        );

        switch (ix) {
            0x00 => {},
            0x01 => self.load(.ptr_bc, .val_xx),
            0x02 => self.load(.ptr_bc, .reg_a),
            0x03 => self.increment(.reg_bc),
            0x04 => self.increment(.reg_b),
            0x05 => self.decrement(.reg_b),
            0x06 => self.load(.reg_b, .val_x),
            0x07 => self.bit_shift(.left, .old_carry, .reg_a),
            0x08 => self.load(.ptr_xx, .reg_sp),
            // 0x09 => TODO ADD
            0x0A => self.load(.reg_a, .ptr_bc),
            0x0B => self.decrement(.reg_bc),
            0x0C => self.increment(.reg_c),
            0x0D => self.decrement(.reg_c),
            0x0E => self.load(.reg_c, .val_x),
            0x0F => self.bit_shift(.right, .old_carry, .reg_a),

            0x10 => {},
            0x11 => self.load(.reg_de, .val_xx),
            0x12 => self.load(.ptr_de, .reg_a),
            0x13 => self.increment(.reg_de),
            0x14 => self.increment(.reg_d),
            0x15 => self.decrement(.reg_d),
            0x16 => self.load(.reg_d, .val_x),
            0x17 => self.bit_shift(.left, .unchanged, .reg_a),
            0x18 => self.jump(.always, .relative, .val_xi),
            // 0x19 => TODO ADD
            0x1A => self.load(.reg_a, .ptr_de),
            0x1B => self.decrement(.reg_de),
            0x1C => self.increment(.reg_e),
            0x1D => self.decrement(.reg_e),
            0x1E => self.load(.reg_e, .val_x),
            0x1F => self.bit_shift(.right, .unchanged, .reg_a),

            0x20 => self.jump(.not_zero, .relative, .val_xi),
            0x21 => self.load(.reg_hl, .val_xx),
            0x22 => self.load_increment(.ptr_hl, .reg_a, .reg_hl),
            0x23 => self.increment(.reg_hl),
            0x24 => self.increment(.reg_h),
            0x25 => self.decrement(.reg_h),
            0x26 => self.load(.reg_h, .val_x),
            // 0x27 => TODO DAA
            0x28 => self.jump(.zero, .relative, .val_xi),
            // 0x29 => TODO ADD
            0x2A => self.load_increment(.reg_a, .ptr_hl, .reg_hl),
            0x2B => self.decrement(.reg_hl),
            0x2C => self.increment(.reg_l),
            0x2D => self.decrement(.reg_l),
            0x2E => self.load(.reg_l, .val_x),
            0x2F => self.complement(.reg_a),

            0x30 => self.jump(.not_carry, .relative, .val_xi),
            0x31 => self.load(.reg_sp, .val_xx),
            0x32 => self.load_decrement(.ptr_hl, .reg_a, .reg_hl),
            0x33 => self.increment(.reg_sp),
            0x34 => self.increment(.ptr_hl),
            0x35 => self.decrement(.ptr_hl),
            0x36 => self.load(.ptr_hl, .val_x),
            0x37 => self.set_carry_flag(),
            0x38 => self.jump(.carry, .relative, .val_xi),
            // 0x39 => TODO ADD
            0x3A => self.load_decrement(.reg_a, .ptr_hl, .reg_hl),
            0x3B => self.decrement(.reg_sp),
            0x3C => self.increment(.reg_a),
            0x3D => self.decrement(.reg_a),
            0x3E => self.load(.reg_a, .val_x),
            0x3F => self.complement_carry_flag(),

            // Begin load heaven

            0x40 => self.load(.reg_b, .reg_b),
            0x41 => self.load(.reg_b, .reg_c),
            0x42 => self.load(.reg_b, .reg_d),
            0x43 => self.load(.reg_b, .reg_e),
            0x44 => self.load(.reg_b, .reg_h),
            0x45 => self.load(.reg_b, .reg_l),
            0x46 => self.load(.reg_b, .ptr_hl),
            0x47 => self.load(.reg_b, .reg_a),
            0x48 => self.load(.reg_c, .reg_b),
            0x49 => self.load(.reg_c, .reg_c),
            0x4A => self.load(.reg_c, .reg_d),
            0x4B => self.load(.reg_c, .reg_e),
            0x4C => self.load(.reg_c, .reg_h),
            0x4D => self.load(.reg_c, .reg_l),
            0x4E => self.load(.reg_c, .ptr_hl),
            0x4F => self.load(.reg_c, .reg_a),

            0x50 => self.load(.reg_d, .reg_b),
            0x51 => self.load(.reg_d, .reg_c),
            0x52 => self.load(.reg_d, .reg_d),
            0x53 => self.load(.reg_d, .reg_e),
            0x54 => self.load(.reg_d, .reg_h),
            0x55 => self.load(.reg_d, .reg_l),
            0x56 => self.load(.reg_d, .ptr_hl),
            0x57 => self.load(.reg_d, .reg_a),
            0x58 => self.load(.reg_e, .reg_b),
            0x59 => self.load(.reg_e, .reg_c),
            0x5A => self.load(.reg_e, .reg_d),
            0x5B => self.load(.reg_e, .reg_e),
            0x5C => self.load(.reg_e, .reg_h),
            0x5D => self.load(.reg_e, .reg_l),
            0x5E => self.load(.reg_e, .ptr_hl),
            0x5F => self.load(.reg_e, .reg_a),

            0x60 => self.load(.reg_h, .reg_b),
            0x61 => self.load(.reg_h, .reg_c),
            0x62 => self.load(.reg_h, .reg_d),
            0x63 => self.load(.reg_h, .reg_e),
            0x64 => self.load(.reg_h, .reg_h),
            0x65 => self.load(.reg_h, .reg_l),
            0x66 => self.load(.reg_h, .ptr_hl),
            0x67 => self.load(.reg_h, .reg_a),
            0x68 => self.load(.reg_l, .reg_b),
            0x69 => self.load(.reg_l, .reg_c),
            0x6A => self.load(.reg_l, .reg_d),
            0x6B => self.load(.reg_l, .reg_e),
            0x6C => self.load(.reg_l, .reg_h),
            0x6D => self.load(.reg_l, .reg_l),
            0x6E => self.load(.reg_l, .ptr_hl),
            0x6F => self.load(.reg_l, .reg_a),

            0x70 => self.load(.ptr_hl, .reg_b),
            0x71 => self.load(.ptr_hl, .reg_c),
            0x72 => self.load(.ptr_hl, .reg_d),
            0x73 => self.load(.ptr_hl, .reg_e),
            0x74 => self.load(.ptr_hl, .reg_h),
            0x75 => self.load(.ptr_hl, .reg_l),
            0x76 => std.os.exit(0), // Halt
            0x77 => self.load(.ptr_hl, .reg_a),
            0x78 => self.load(.reg_a, .reg_b),
            0x79 => self.load(.reg_a, .reg_c),
            0x7A => self.load(.reg_a, .reg_d),
            0x7B => self.load(.reg_a, .reg_e),
            0x7C => self.load(.reg_a, .reg_h),
            0x7D => self.load(.reg_a, .reg_l),
            0x7E => self.load(.reg_a, .ptr_hl),
            0x7F => self.load(.reg_a, .reg_a),

            // Begin accumulator madness

            0xEA => self.load(.ptr_xx, .reg_a),

            0xF3 => self.interrupts(false),
            0xFA => self.load(.reg_a, .ptr_xx),
            0xFB => self.interrupts(true),

            else => std.debug.panic(
                "Unimplemented '{s}' @ ${X:0>4}\n",
                .{ details.mnemonic, self.registers.pc },
            ),
        }

        // Advance the program counter
        self.registers.pc += details.byte_length;

        return details.clock_cycles;
    }

    // TODO Make redundant enums anonymous parameter types.
    // See: https://github.com/ziglang/zig/issues/3707

    const BitShiftDirection = enum { left, right };
    const BitShiftOldBitMode = enum { reset, old_carry, unchanged };

    fn bit_shift(
        self: *Self,
        comptime direction: BitShiftDirection,
        comptime old_bit_mode: BitShiftOldBitMode,
        comptime arg: IxArgument,
    ) void {
        const reg = self.get_8bit_argument_address(arg);
        const val = self.get_8bit_argument_value(arg);

        const old_bit = switch (direction) {
            .left => val & 0b1,
            .right => val >> 7,
        };

        const shift_amount = switch (direction) {
            .left => 0,
            .right => 7,
        };

        reg.* = switch (direction) {
            .left => val << 1,
            .right => val >> 1,
        };

        reg.* |= switch (old_bit_mode) {
            .reset => 0b0000_0000, // Do nothing, modern ISAs do the work for us.
            .unchanged => old_bit << shift_amount,
            .old_carry => @intCast(u8, self.registers.af.ind.f.cary) << shift_amount,
        };

        self.set_flag("zero", reg.* == 0);
        self.set_flag("nsub", false);
        self.set_flag("half", false);
        self.set_flag("cary", old_bit == 1);
    }

    fn bit_swap(self: *Self, comptime arg: IxArgument) void {
        const reg = self.get_8bit_argument_address(arg);
        const val = (reg.* << 4) | (reg.* >> 4);
        self.set_flag("zero", val == 0);
        self.set_flag("nsub", false);
        self.set_flag("half", false);
        self.set_flag("cary", false);
        reg.* = val;
    }

    fn bit_test(self: *Self, comptime bit: u2, comptime arg: IxArgument) void {
        const byte = self.get_8bit_argument_value(arg);
        self.set_flag("zero", (byte & (0b1 << bit)) == 0);
        self.set_flag("nsub", false);
        self.set_flag("half", true);
    }

    fn bit_set(self: *Self, comptime bit: u2, comptime arg: IxArgument) void {
        const byte_ptr = self.get_8bit_argument_address(arg);
        byte_ptr.* = byte_ptr.* | (0b1 << bit);
    }

    fn bit_reset(self: *Self, comptime bit: u2, comptime arg: IxArgument) void {
        const byte_ptr = self.get_8bit_argument_address(arg);
        const bit_mask = @as(u8, 0b1 << bit);
        byte_ptr.* = byte_ptr.* & ~bit_mask;
    }

    fn step_prefixed_instruction(self: *Self) usize {
        const ix = self.ram.read(u8, self.registers.pc);

        const details = prefixed_instructions_details(ix) orelse std.debug.panic(
            "Unknown '0xCB 0x{X:0>2}' @ ${X:0>4}\n",
            .{ ix, self.registers.pc },
        );

        const repeated_registers = comptime [_]IxArgument{
            .reg_b, .reg_c, .reg_d,  .reg_e,
            .reg_h, .reg_l, .ptr_hl, .reg_a,
        };

        // TODO Replace this with an inline switch.
        // See: https://github.com/ziglang/zig/issues/7224

        inline for (std.mem.zeroes([8]void)) |_, op| if (op + 0x00 == ix) {
            self.bit_shift(.left, .unchanged, repeated_registers[op % 8]);
        };

        inline for (std.mem.zeroes([8]void)) |_, op| if (op + 0x08 == ix) {
            self.bit_shift(.right, .unchanged, repeated_registers[op % 8]);
        };

        inline for (std.mem.zeroes([8]void)) |_, op| if (op + 0x10 == ix) {
            self.bit_shift(.left, .old_carry, repeated_registers[op % 8]);
        };

        inline for (std.mem.zeroes([8]void)) |_, op| if (op + 0x18 == ix) {
            self.bit_shift(.right, .old_carry, repeated_registers[op % 8]);
        };

        inline for (std.mem.zeroes([8]void)) |_, op| if (op + 0x20 == ix) {
            self.bit_shift(.left, .reset, repeated_registers[op % 8]);
        };

        inline for (std.mem.zeroes([8]void)) |_, op| if (op + 0x28 == ix) {
            self.bit_shift(.right, .unchanged, repeated_registers[op % 8]);
        };

        inline for (std.mem.zeroes([8]void)) |_, op| if (op + 0x30 == ix) {
            self.bit_swap(repeated_registers[op % 8]);
        };

        inline for (std.mem.zeroes([8]void)) |_, op| if (op + 0x38 == ix) {
            self.bit_shift(.right, .reset, repeated_registers[op % 8]);
        };

        inline for (std.mem.zeroes([8 * 8]void)) |_, op| if (op + 0x40 == ix) {
            const bit = @truncate(u2, op / 8);
            const arg = repeated_registers[op % 8];
            self.bit_test(bit, arg);
        };

        inline for (std.mem.zeroes([8 * 8]void)) |_, op| if (op + 0x80 == ix) {
            const bit = @truncate(u2, op / 8);
            const arg = repeated_registers[op % 8];
            self.bit_reset(bit, arg);
        };

        inline for (std.mem.zeroes([8 * 8]void)) |_, op| if (op + 0xC0 == ix) {
            const bit = @truncate(u2, op / 8);
            const arg = repeated_registers[op % 8];
            self.bit_set(bit, arg);
        };

        // Advance the program counter
        self.registers.pc += details.byte_length;

        return details.clock_cycles;
    }

    pub fn print_state() void {
        std.debug.print("\n\n", .{});

        std.debug.print("${X:0>4}: {s}\n", .{ self.registers.pc, details.mnemonic });

        std.debug.print("Registers: A = {}, B = {}, C = {}, D = {}, E = {}, H = {}, L = {}\n", .{
            self.registers.af.ind.a,
            self.registers.bc.ind.b,
            self.registers.bc.ind.c,
            self.registers.de.ind.d,
            self.registers.de.ind.e,
            self.registers.hl.ind.h,
            self.registers.hl.ind.l,
        });

        std.debug.print("16-bit Registers: AF = {}, BC = {}, DE = {}, HL = {}, SP = 0x{X:0>4}\n", .{
            self.registers.af.all,
            self.registers.bc.all,
            self.registers.de.all,
            self.registers.hl.all,
            self.registers.sp,
        });

        std.debug.print("Flags: {}\n", .{self.registers.af.ind.f});

        std.debug.print("PC (0x{X:0>4}) points to: [ {X:0>2} {X:0>2} {X:0>2} {X:0>2} {X:0>2} ]\n", .{
            self.registers.pc,
            self.ram.buffer[self.registers.pc + 0],
            self.ram.buffer[self.registers.pc + 1],
            self.ram.buffer[self.registers.pc + 2],
            self.ram.buffer[self.registers.pc + 3],
            self.ram.buffer[self.registers.pc + 4],
        });
    }
};
