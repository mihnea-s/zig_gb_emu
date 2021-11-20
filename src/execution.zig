const std = @import("std");

const MMU = @import("memory.zig");

usingnamespace @import("instructions.zig");

const Self = @This();

const PseudoRNG = std.rand.DefaultPrng;
const InstructionMap = std.AutoHashMap(u8, Instruction);

const IxArgument = enum {
    reg_a,
    reg_f,

    reg_b,
    reg_c,

    reg_d,
    reg_e,

    reg_h,
    reg_l,

    reg_af,
    reg_bc,
    reg_de,
    reg_hl,

    reg_sp,
    reg_pc,

    ptr_de,
    ptr_bc,
    ptr_hl,

    mem_u8,
    mem_i8,
    mem_u16,
    ptr_mem_u16,

    ptr_high_c,
    ptr_high_u8,
};

const IxCondition = enum {
    always,
    zero,
    not_zero,
    carry,
    not_carry,
};

mmu: *MMU,
allocator: *std.mem.Allocator,

registers: Registers,
pseudorng: PseudoRNG,
timer_div_clock: usize,
timer_conf_clock: usize,

pub fn init(mmu: *MMU, allocator: *std.mem.Allocator) !Self {
    return Self{
        .mmu = mmu,
        .allocator = allocator,

        .registers = Registers{},
        .pseudorng = PseudoRNG.init(@intCast(u64, std.time.milliTimestamp())),
        .timer_div_clock = 0,
        .timer_conf_clock = 0,
    };
}

pub fn deinit(self: *Self) void {}

fn flags(self: *Self) *FlagRegister {
    return &self.registers.af.ind.f;
}

fn fulfills_condition(self: *Self, comptime cond: IxCondition) bool {
    return switch (cond) {
        .always => true,
        .zero => self.flags().zero,
        .not_zero => !self.flags().zero,
        .carry => self.flags().carry,
        .not_carry => !self.flags().carry,
    };
}

fn is_arg_16(comptime target: IxArgument) bool {
    return switch (target) {
        .reg_af, .reg_bc, .reg_de, .reg_hl, .reg_sp, .mem_u16 => true,
        else => false,
    };
}

fn get_arg_8(self: *Self, comptime target: IxArgument) u8 {
    return switch (target) {
        .reg_a => self.registers.af.ind.a,
        .reg_f => self.registers.af.ind.f,
        .reg_b => self.registers.bc.ind.b,
        .reg_c => self.registers.bc.ind.c,
        .reg_d => self.registers.de.ind.d,
        .reg_e => self.registers.de.ind.e,
        .reg_h => self.registers.hl.ind.h,
        .reg_l => self.registers.hl.ind.l,
        .ptr_bc => self.mmu.read(u8, self.registers.bc.all),
        .ptr_de => self.mmu.read(u8, self.registers.de.all),
        .ptr_hl => self.mmu.read(u8, self.registers.hl.all),
        .mem_u8 => self.mmu.read(u8, self.registers.pc +% 1),
        .ptr_mem_u16 => self.mmu.read(u8, self.get_arg_16(.mem_u16)),
        .ptr_high_c => self.mmu.read(u8, @as(u16, 0xFF00) +% self.registers.bc.ind.c),
        .ptr_high_u8 => self.mmu.read(u8, @as(u16, 0xFF00) +% self.get_arg_8(.mem_u8)),
        else => @compileError("invalid 8bit argument"),
    };
}

fn get_arg_8_signed(self: *Self) i8 {
    return self.mmu.read(i8, self.registers.pc +% 1);
}

fn set_arg_8(self: *Self, comptime target: IxArgument, value: u8) void {
    return switch (target) {
        .reg_a => self.registers.af.ind.a = value,
        .reg_f => self.registers.af.ind.f = value,
        .reg_b => self.registers.bc.ind.b = value,
        .reg_c => self.registers.bc.ind.c = value,
        .reg_d => self.registers.de.ind.d = value,
        .reg_e => self.registers.de.ind.e = value,
        .reg_h => self.registers.hl.ind.h = value,
        .reg_l => self.registers.hl.ind.l = value,
        .ptr_bc => self.mmu.write(u8, self.registers.bc.all, value),
        .ptr_de => self.mmu.write(u8, self.registers.de.all, value),
        .ptr_hl => self.mmu.write(u8, self.registers.hl.all, value),
        .mem_u8 => self.mmu.write(u8, self.registers.pc +% 1, value),
        .ptr_mem_u16 => self.mmu.write(u8, self.get_arg_16(.mem_u16), value),
        .ptr_high_c => self.mmu.write(u8, @as(u16, 0xFF00) +% self.registers.bc.ind.c, value),
        .ptr_high_u8 => self.mmu.write(u8, @as(u16, 0xFF00) +% self.get_arg_8(.mem_u8), value),
        else => self.get_8bit_argument_address(target).*,
    };
}

fn get_arg_16(self: *Self, comptime target: IxArgument) u16 {
    return switch (target) {
        .reg_af => self.registers.af.all,
        .reg_bc => self.registers.bc.all,
        .reg_de => self.registers.de.all,
        .reg_hl => self.registers.hl.all,
        .reg_sp => self.registers.sp,
        .reg_pc => self.registers.pc,
        .ptr_bc => self.mmu.read(u16, self.registers.bc.all),
        .ptr_de => self.mmu.read(u16, self.registers.de.all),
        .ptr_hl => self.mmu.read(u16, self.registers.hl.all),
        .mem_u16 => self.mmu.read(u16, self.registers.pc +% 1),
        .ptr_mem_u16 => self.mmu.read(u16, self.get_arg_16(.mem_u16)),
        else => @compileError("invalid 16bit argument"),
    };
}

fn set_arg_16(self: *Self, comptime target: IxArgument, value: u16) void {
    return switch (target) {
        .reg_af => self.registers.af.all = value,
        .reg_bc => self.registers.bc.all = value,
        .reg_de => self.registers.de.all = value,
        .reg_hl => self.registers.hl.all = value,
        .reg_sp => self.registers.sp = value,
        .reg_pc => self.registers.pc = value,
        .ptr_bc => self.mmu.write(u16, self.registers.bc.all, value),
        .ptr_de => self.mmu.write(u16, self.registers.de.all, value),
        .ptr_hl => self.mmu.write(u16, self.registers.hl.all, value),
        .mem_u16 => self.mmu.write(u16, self.registers.pc +% 1, value),
        .ptr_mem_u16 => self.mmu.write(u16, self.get_arg_16(.mem_u16), value),
        else => @compileError("invalid 16bit argument"),
    };
}

fn increment(self: *Self, comptime arg: IxArgument) void {
    if (comptime is_arg_16(arg)) {
        self.set_arg_16(arg, self.get_arg_16(arg) +% 1);
    } else {
        const value = self.get_arg_8(arg);
        self.set_arg_8(arg, value +% 1);

        self.flags().zero = value == 0xFF;
        self.flags().nsub = false;
        self.flags().half = (value & 0xF) == 0xF;
    }
}

fn decrement(self: *Self, comptime arg: IxArgument) void {
    if (comptime is_arg_16(arg)) {
        self.set_arg_16(arg, self.get_arg_16(arg) -% 1);
    } else {
        const value = self.get_arg_8(arg);
        self.set_arg_8(arg, value -% 1);

        self.flags().zero = value == 0x01;
        self.flags().nsub = true;
        self.flags().half = (value & 0xF) == 0x0;
    }
}

fn load(self: *Self, comptime to: IxArgument, comptime from: IxArgument) void {
    if (comptime is_arg_16(from)) {
        self.set_arg_16(to, self.get_arg_16(from));
    } else {
        self.set_arg_8(to, self.get_arg_8(from));
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
    comptime incr: IxArgument,
) void {
    self.load(to, from);
    self.increment(incr);
}

// TODO Make redundant enums anonymous parameter types.
// See: https://github.com/ziglang/zig/issues/3707

const ArithmeticOperation = enum {
    addition,
    addition_carry,
    subtract,
    subtract_carry,
    bitwise_and,
    bitwise_or,
    bitwise_xor,
    compare,
};

fn arithmetic(
    self: *Self,
    comptime lhs: IxArgument,
    comptime oper: ArithmeticOperation,
    comptime rhs: IxArgument,
) void {
    comptime if (is_arg_16(lhs) or is_arg_16(rhs)) {
        @compileError("arithmetic arguments can only be 8-bit");
    };

    const left = self.get_arg_8(lhs);
    const right = self.get_arg_8(rhs);
    const carry = @boolToInt(self.registers.af.ind.f.carry);

    const result = switch (oper) {
        .addition => left +% right,
        .addition_carry => left +% (right +% carry),
        .subtract => left -% right,
        .subtract_carry => left -% (right +% carry),
        .bitwise_and => left & right,
        .bitwise_or => left | right,
        .bitwise_xor => left ^ right,
        .compare => left -% right,
    };

    self.flags().zero = result == 0;

    self.flags().nsub = switch (oper) {
        .subtract, .subtract_carry, .compare => true,
        else => false,
    };

    self.flags().half = switch (oper) {
        .addition => @intCast(u16, left) + right > 0xF,
        .addition_carry => @intCast(u16, left) + right + carry > 0xF,
        .subtract, .compare => (right & 0xF) > left & 0xF,
        .subtract_carry => (right & 0xF) > (left & 0xF) + carry,
        .bitwise_and => true,
        .bitwise_or, .bitwise_xor => false,
    };

    var spare: u8 = undefined;

    self.flags().carry = switch (oper) {
        .addition, .addition_carry => @addWithOverflow(u8, left, right, &spare),
        .subtract, .subtract_carry, .compare => @subWithOverflow(u8, left, right, &spare),
        else => false,
    };

    if (oper != .compare) {
        self.set_arg_8(lhs, result);
    }
}

fn jump_relative(self: *Self, comptime cond: IxCondition, comptime arg: IxArgument) void {
    if (!self.fulfills_condition(cond)) {
        return;
    }

    if (arg == .mem_i8) {
        const offset = @intCast(i16, self.get_arg_8_signed());
        self.registers.pc +%= @bitCast(u16, offset);
    } else {
        self.registers.pc +%= self.get_8bit_argument_value(arg);
    }
}

fn jump_position(self: *Self, comptime cond: IxCondition, comptime arg: IxArgument) void {
    const byte_length = if (arg == .mem_u16) 3 else 1;

    if (self.fulfills_condition(cond)) {
        self.registers.pc = self.get_arg_16(arg);
    } else {
        self.registers.pc +%= byte_length;
    }
}

fn push_stack(self: *Self, comptime arg: IxArgument) void {
    self.registers.sp -%= 2;
    self.mmu.write(u16, self.registers.sp, self.get_arg_16(arg));
}

fn pop_stack(self: *Self, comptime arg: IxArgument) void {
    self.set_arg_16(arg, self.mmu.read(u16, self.registers.sp));
    self.registers.sp +%= 2;

    // For some reason lower nibble of F is hardwired to 0.
    if (arg == .reg_af) self.registers.af.ind.f.unused = 0;
}

fn call(self: *Self, comptime cond: IxCondition, comptime arg: IxArgument) void {
    const byte_length = 3;

    if (self.fulfills_condition(cond)) {
        self.registers.sp -%= 2;
        self.mmu.write(u16, self.registers.sp, self.registers.pc +% byte_length);
        self.load(.reg_pc, arg);
    } else {
        self.registers.pc +%= byte_length;
    }
}

fn return_call(self: *Self, comptime cond: IxCondition) void {
    if (self.fulfills_condition(cond)) {
        self.pop_stack(.reg_pc);
    } else {
        self.registers.pc +%= 1;
    }
}

fn return_call_ei(self: *Self) void {
    self.return_call(.always);
    self.interrupt_master_enable(true);
}

// TODO Make redundant enums anonymous parameter types.
// See: https://github.com/ziglang/zig/issues/3707

const RestartAddr = enum(u8) {
    x00 = 0x00,
    x08 = 0x08,
    x10 = 0x10,
    x18 = 0x18,
    x20 = 0x20,
    x28 = 0x28,
    x30 = 0x30,
    x38 = 0x38,
    x40 = 0x40,
    x48 = 0x48,
    x50 = 0x50,
    x58 = 0x58,
    x60 = 0x60,
};

fn restart(self: *Self, comptime addr: RestartAddr) void {
    self.push_stack(.reg_pc);
    self.registers.pc = @enumToInt(addr);
}

fn decimal_adjust(self: *Self) void {
    if (self.flags().nsub) {
        if (self.flags().carry) self.registers.af.ind.a -%= 0x60;
        if (self.flags().half) self.registers.af.ind.a -%= 0x06;
    } else {
        if (self.flags().carry or self.registers.af.ind.a > 0x99) {
            self.registers.af.ind.a +%= 0x60;
            self.flags().carry = true;
        }

        if (self.flags().half or (self.registers.af.ind.a & 0x0F) > 0x09) {
            self.registers.af.ind.a +%= 0x06;
        }
    }

    self.flags().zero = self.registers.af.ind.a == 0;
    self.flags().half = false;
}

fn complement(self: *Self, comptime arg: IxArgument) void {
    self.flags().nsub = true;
    self.flags().half = true;
    self.set_arg_8(arg, ~self.get_arg_8(arg));
}

fn complement_carry_flag(self: *Self) void {
    self.flags().nsub = false;
    self.flags().half = false;
    self.flags().carry = !self.flags().carry;
}

fn set_carry_flag(self: *Self) void {
    self.flags().nsub = false;
    self.flags().half = false;
    self.flags().carry = true;
}

fn add_to_hl_register(self: *Self, comptime arg: IxArgument) void {
    const hl = &self.registers.hl.all;
    const value = self.get_arg_16(arg);

    var result: u16 = undefined;
    var carried = @addWithOverflow(u16, hl.*, value, &result);

    self.flags().nsub = false;
    self.flags().half = (hl.* & 0xFF) + (value & 0xFF) >= 0xFF;
    self.flags().carry = carried;

    hl.* = result;
}

// TODO Make redundant enums anonymous parameter types.
// See: https://github.com/ziglang/zig/issues/3707

const StackPointerModifyOper = enum {
    shift,
    load_to_hl,
};

fn stack_pointer_modify(self: *Self, comptime oper: StackPointerModifyOper) void {
    const offset = self.get_arg_8_signed();
    const cast_offset = @bitCast(u16, @intCast(i16, offset));

    var result: u16 = undefined;

    const overflow = @addWithOverflow(u16, self.registers.sp, cast_offset, &result);

    const half_overflow = (self.registers.sp & 0xFF) + @bitCast(u8, offset) > 0xFF;

    self.flags().zero = false;
    self.flags().nsub = false;
    self.flags().half = half_overflow;
    self.flags().carry = overflow;

    switch (oper) {
        .shift => self.registers.sp = result,
        .load_to_hl => self.registers.hl.all = result,
    }
}

fn interrupt_master_enable(self: *Self, comptime enabled: bool) void {
    self.registers.ime = enabled;
}

pub fn step_instruction(self: *Self) usize {
    const ix = self.mmu.read(u8, self.registers.pc);

    if (ix == 0xCB) {
        self.registers.pc += 1;
        return self.step_prefixed_instruction();
    }

    const details = instructions_details(ix) orelse std.debug.panic(
        "Unknown '0x{X:0>2}' @ ${X:0>4}\n",
        .{ ix, self.registers.pc },
    );

    switch (ix) {
        // --------------------
        // Control instructions
        // --------------------

        0x00 => {}, // Noop
        0x10 => {}, // Stop
        0x76 => {}, // Halt
        0xF3 => self.interrupt_master_enable(false),
        0xFB => self.interrupt_master_enable(true),
        0xCB => unreachable, // Second table prefix

        // ---------------------
        // Loads / Store / Move
        // ---------------------

        0x06 => self.load(.reg_b, .mem_u8),
        0x0E => self.load(.reg_c, .mem_u8),
        0x16 => self.load(.reg_d, .mem_u8),
        0x1E => self.load(.reg_e, .mem_u8),
        0x26 => self.load(.reg_h, .mem_u8),
        0x2E => self.load(.reg_l, .mem_u8),
        0x36 => self.load(.ptr_hl, .mem_u8),
        0x3E => self.load(.reg_a, .mem_u8),

        0x0A => self.load(.reg_a, .ptr_bc),
        0x1A => self.load(.reg_a, .ptr_de),
        0x2A => self.load_increment(.reg_a, .ptr_hl, .reg_hl),
        0x3A => self.load_decrement(.reg_a, .ptr_hl, .reg_hl),

        0x02 => self.load(.ptr_bc, .reg_a),
        0x12 => self.load(.ptr_de, .reg_a),
        0x22 => self.load_increment(.ptr_hl, .reg_a, .reg_hl),
        0x32 => self.load_decrement(.ptr_hl, .reg_a, .reg_hl),

        0x01 => self.load(.reg_bc, .mem_u16),
        0x11 => self.load(.reg_de, .mem_u16),
        0x21 => self.load(.reg_hl, .mem_u16),
        0x31 => self.load(.reg_sp, .mem_u16),
        0x08 => self.load(.ptr_mem_u16, .reg_sp),

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
        0x77 => self.load(.ptr_hl, .reg_a),
        0x78 => self.load(.reg_a, .reg_b),
        0x79 => self.load(.reg_a, .reg_c),
        0x7A => self.load(.reg_a, .reg_d),
        0x7B => self.load(.reg_a, .reg_e),
        0x7C => self.load(.reg_a, .reg_h),
        0x7D => self.load(.reg_a, .reg_l),
        0x7E => self.load(.reg_a, .ptr_hl),
        0x7F => self.load(.reg_a, .reg_a),

        0xEA => self.load(.ptr_mem_u16, .reg_a),
        0xFA => self.load(.reg_a, .ptr_mem_u16),
        0xF9 => self.load(.reg_sp, .reg_hl),

        0xE0 => self.load(.ptr_high_u8, .reg_a),
        0xF0 => self.load(.reg_a, .ptr_high_u8),
        0xE2 => self.load(.ptr_high_c, .reg_a),
        0xF2 => self.load(.reg_a, .ptr_high_c),

        // -----------------------
        // Increments / Decrements
        // -----------------------

        0x04 => self.increment(.reg_b),
        0x0C => self.increment(.reg_c),
        0x14 => self.increment(.reg_d),
        0x1C => self.increment(.reg_e),
        0x2C => self.increment(.reg_l),
        0x24 => self.increment(.reg_h),
        0x3C => self.increment(.reg_a),

        0x05 => self.decrement(.reg_b),
        0x0D => self.decrement(.reg_c),
        0x15 => self.decrement(.reg_d),
        0x1D => self.decrement(.reg_e),
        0x2D => self.decrement(.reg_l),
        0x25 => self.decrement(.reg_h),
        0x3D => self.decrement(.reg_a),

        0x03 => self.increment(.reg_bc),
        0x13 => self.increment(.reg_de),
        0x23 => self.increment(.reg_hl),
        0x34 => self.increment(.ptr_hl),
        0x33 => self.increment(.reg_sp),

        0x0B => self.decrement(.reg_bc),
        0x1B => self.decrement(.reg_de),
        0x2B => self.decrement(.reg_hl),
        0x35 => self.decrement(.ptr_hl),
        0x3B => self.decrement(.reg_sp),

        // ----------------------
        // Arithmetic Operations
        // ----------------------

        0x80 => self.arithmetic(.reg_a, .addition, .reg_b),
        0x81 => self.arithmetic(.reg_a, .addition, .reg_c),
        0x82 => self.arithmetic(.reg_a, .addition, .reg_d),
        0x83 => self.arithmetic(.reg_a, .addition, .reg_e),
        0x84 => self.arithmetic(.reg_a, .addition, .reg_h),
        0x85 => self.arithmetic(.reg_a, .addition, .reg_l),
        0x86 => self.arithmetic(.reg_a, .addition, .ptr_hl),
        0x87 => self.arithmetic(.reg_a, .addition, .reg_a),
        0x88 => self.arithmetic(.reg_a, .addition_carry, .reg_b),
        0x89 => self.arithmetic(.reg_a, .addition_carry, .reg_c),
        0x8A => self.arithmetic(.reg_a, .addition_carry, .reg_d),
        0x8B => self.arithmetic(.reg_a, .addition_carry, .reg_e),
        0x8C => self.arithmetic(.reg_a, .addition_carry, .reg_h),
        0x8D => self.arithmetic(.reg_a, .addition_carry, .reg_l),
        0x8E => self.arithmetic(.reg_a, .addition_carry, .ptr_hl),
        0x8F => self.arithmetic(.reg_a, .addition_carry, .reg_a),

        0x90 => self.arithmetic(.reg_a, .subtract, .reg_b),
        0x91 => self.arithmetic(.reg_a, .subtract, .reg_c),
        0x92 => self.arithmetic(.reg_a, .subtract, .reg_d),
        0x93 => self.arithmetic(.reg_a, .subtract, .reg_e),
        0x94 => self.arithmetic(.reg_a, .subtract, .reg_h),
        0x95 => self.arithmetic(.reg_a, .subtract, .reg_l),
        0x96 => self.arithmetic(.reg_a, .subtract, .ptr_hl),
        0x97 => self.arithmetic(.reg_a, .subtract, .reg_a),
        0x98 => self.arithmetic(.reg_a, .subtract_carry, .reg_b),
        0x99 => self.arithmetic(.reg_a, .subtract_carry, .reg_c),
        0x9A => self.arithmetic(.reg_a, .subtract_carry, .reg_d),
        0x9B => self.arithmetic(.reg_a, .subtract_carry, .reg_e),
        0x9C => self.arithmetic(.reg_a, .subtract_carry, .reg_h),
        0x9D => self.arithmetic(.reg_a, .subtract_carry, .reg_l),
        0x9E => self.arithmetic(.reg_a, .subtract_carry, .ptr_hl),
        0x9F => self.arithmetic(.reg_a, .subtract_carry, .reg_a),

        0xA0 => self.arithmetic(.reg_a, .bitwise_and, .reg_b),
        0xA1 => self.arithmetic(.reg_a, .bitwise_and, .reg_c),
        0xA2 => self.arithmetic(.reg_a, .bitwise_and, .reg_d),
        0xA3 => self.arithmetic(.reg_a, .bitwise_and, .reg_e),
        0xA4 => self.arithmetic(.reg_a, .bitwise_and, .reg_h),
        0xA5 => self.arithmetic(.reg_a, .bitwise_and, .reg_l),
        0xA6 => self.arithmetic(.reg_a, .bitwise_and, .ptr_hl),
        0xA7 => self.arithmetic(.reg_a, .bitwise_and, .reg_a),
        0xA8 => self.arithmetic(.reg_a, .bitwise_xor, .reg_b),
        0xA9 => self.arithmetic(.reg_a, .bitwise_xor, .reg_c),
        0xAA => self.arithmetic(.reg_a, .bitwise_xor, .reg_d),
        0xAB => self.arithmetic(.reg_a, .bitwise_xor, .reg_e),
        0xAC => self.arithmetic(.reg_a, .bitwise_xor, .reg_h),
        0xAD => self.arithmetic(.reg_a, .bitwise_xor, .reg_l),
        0xAE => self.arithmetic(.reg_a, .bitwise_xor, .ptr_hl),
        0xAF => self.arithmetic(.reg_a, .bitwise_xor, .reg_a),

        0xB0 => self.arithmetic(.reg_a, .bitwise_or, .reg_b),
        0xB1 => self.arithmetic(.reg_a, .bitwise_or, .reg_c),
        0xB2 => self.arithmetic(.reg_a, .bitwise_or, .reg_d),
        0xB3 => self.arithmetic(.reg_a, .bitwise_or, .reg_e),
        0xB4 => self.arithmetic(.reg_a, .bitwise_or, .reg_h),
        0xB5 => self.arithmetic(.reg_a, .bitwise_or, .reg_l),
        0xB6 => self.arithmetic(.reg_a, .bitwise_or, .ptr_hl),
        0xB7 => self.arithmetic(.reg_a, .bitwise_or, .reg_a),
        0xB8 => self.arithmetic(.reg_a, .compare, .reg_b),
        0xB9 => self.arithmetic(.reg_a, .compare, .reg_c),
        0xBA => self.arithmetic(.reg_a, .compare, .reg_d),
        0xBB => self.arithmetic(.reg_a, .compare, .reg_e),
        0xBC => self.arithmetic(.reg_a, .compare, .reg_h),
        0xBD => self.arithmetic(.reg_a, .compare, .reg_l),
        0xBE => self.arithmetic(.reg_a, .compare, .ptr_hl),
        0xBF => self.arithmetic(.reg_a, .compare, .reg_a),

        0xC6 => self.arithmetic(.reg_a, .addition, .mem_u8),
        0xCE => self.arithmetic(.reg_a, .addition_carry, .mem_u8),
        0xD6 => self.arithmetic(.reg_a, .subtract, .mem_u8),
        0xDE => self.arithmetic(.reg_a, .subtract_carry, .mem_u8),
        0xE6 => self.arithmetic(.reg_a, .bitwise_and, .mem_u8),
        0xEE => self.arithmetic(.reg_a, .bitwise_xor, .mem_u8),
        0xF6 => self.arithmetic(.reg_a, .bitwise_or, .mem_u8),
        0xFE => self.arithmetic(.reg_a, .compare, .mem_u8),

        0x09 => self.add_to_hl_register(.reg_bc),
        0x19 => self.add_to_hl_register(.reg_de),
        0x29 => self.add_to_hl_register(.reg_hl),
        0x39 => self.add_to_hl_register(.reg_sp),
        0xE8 => self.stack_pointer_modify(.shift),
        0xF8 => self.stack_pointer_modify(.load_to_hl),

        // ----------------------
        // Decimal instruction
        // ----------------------

        0x27 => self.decimal_adjust(),
        0x2F => self.complement(.reg_a),
        0x37 => self.set_carry_flag(),
        0x3F => self.complement_carry_flag(),

        // ----------------------
        // RLCA / RLA instruction
        // ----------------------

        0x07 => self.bit_shift(.left, .old_bit, .reg_a),
        0x17 => self.bit_shift(.left, .old_carry, .reg_a),
        0x0F => self.bit_shift(.right, .old_bit, .reg_a),
        0x1F => self.bit_shift(.right, .old_carry, .reg_a),

        // -----------------------
        // Jumps / Calls / Returns
        // -----------------------

        0xC5 => self.push_stack(.reg_bc),
        0xD5 => self.push_stack(.reg_de),
        0xE5 => self.push_stack(.reg_hl),
        0xF5 => self.push_stack(.reg_af),

        0xC1 => self.pop_stack(.reg_bc),
        0xD1 => self.pop_stack(.reg_de),
        0xE1 => self.pop_stack(.reg_hl),
        0xF1 => self.pop_stack(.reg_af),

        0x18 => self.jump_relative(.always, .mem_i8),
        0x28 => self.jump_relative(.zero, .mem_i8),
        0x20 => self.jump_relative(.not_zero, .mem_i8),
        0x38 => self.jump_relative(.carry, .mem_i8),
        0x30 => self.jump_relative(.not_carry, .mem_i8),
        0xC3 => self.jump_position(.always, .mem_u16),
        0xCA => self.jump_position(.zero, .mem_u16),
        0xC2 => self.jump_position(.not_zero, .mem_u16),
        0xDA => self.jump_position(.carry, .mem_u16),
        0xD2 => self.jump_position(.not_carry, .mem_u16),
        0xE9 => self.jump_position(.always, .reg_hl),

        0xCD => self.call(.always, .mem_u16),
        0xCC => self.call(.zero, .mem_u16),
        0xC4 => self.call(.not_zero, .mem_u16),
        0xDC => self.call(.carry, .mem_u16),
        0xD4 => self.call(.not_carry, .mem_u16),

        0xD9 => self.return_call_ei(),
        0xC9 => self.return_call(.always),
        0xC8 => self.return_call(.zero),
        0xC0 => self.return_call(.not_zero),
        0xD8 => self.return_call(.carry),
        0xD0 => self.return_call(.not_carry),

        0xC7 => self.restart(.x00),
        0xCF => self.restart(.x08),
        0xD7 => self.restart(.x10),
        0xDF => self.restart(.x18),
        0xE7 => self.restart(.x20),
        0xEF => self.restart(.x28),
        0xF7 => self.restart(.x30),
        0xFF => self.restart(.x38),

        else => std.debug.panic(
            "Unimplemented '{s}' @ ${X:0>4}\n",
            .{ details.mnemonic, self.registers.pc },
        ),
    }

    // Advance the program counter
    self.registers.pc +%= details.byte_length;

    return details.clock_cycles;
}

// TODO Make redundant enums anonymous parameter types.
// See: https://github.com/ziglang/zig/issues/3707

const BitShiftDirection = enum { left, right };
const BitShiftNewBitValue = enum { reset, old_carry, old_bit, previous_bit };

fn bit_shift(
    self: *Self,
    comptime direction: BitShiftDirection,
    comptime new_bit_value: BitShiftNewBitValue,
    comptime arg: IxArgument,
) void {
    const value = self.get_arg_8(arg);

    const old_carry = @boolToInt(self.flags().carry);

    const old_bit = switch (direction) {
        .left => (value >> 7) & 0b1,
        .right => (value >> 0) & 0b1,
    };

    var result = switch (direction) {
        .left => value << 1,
        .right => value >> 1,
    };

    const shift_amount = switch (direction) {
        .left => 0,
        .right => 7,
    };

    result |= switch (new_bit_value) {
        .reset => 0b0000_0000, // Do nothing, modern ISAs do the work for us.
        .old_bit => old_bit << shift_amount,
        .old_carry => @intCast(u8, old_carry) << shift_amount,
        .previous_bit => value & (0b1 << shift_amount),
    };

    self.flags().zero = result == 0;
    self.flags().nsub = false;
    self.flags().half = false;
    self.flags().carry = old_bit == 1;

    self.set_arg_8(arg, result);
}

fn bit_swap(self: *Self, comptime arg: IxArgument) void {
    const value = self.get_arg_8(arg);
    const result = (value << 4) | (value >> 4);

    self.flags().zero = result == 0;
    self.flags().nsub = false;
    self.flags().half = false;
    self.flags().carry = false;

    self.set_arg_8(arg, result);
}

fn bit_test(self: *Self, comptime bit: u3, comptime arg: IxArgument) void {
    const byte = self.get_arg_8(arg);

    self.flags().zero = (byte & (0b1 << bit)) == 0;
    self.flags().nsub = false;
    self.flags().half = true;
}

fn bit_set(self: *Self, comptime bit: u3, comptime arg: IxArgument) void {
    const value = self.get_arg_8(arg);
    self.set_arg_8(arg, value | (0b1 << bit));
}

fn bit_reset(self: *Self, comptime bit: u3, comptime arg: IxArgument) void {
    const value = self.get_arg_8(arg);
    const mask: u8 = 0b1 << bit;
    self.set_arg_8(arg, value & ~mask);
}

fn step_prefixed_instruction(self: *Self) usize {
    const ix = self.mmu.read(u8, self.registers.pc);

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
        self.bit_shift(.left, .old_bit, repeated_registers[op % 8]);
    };

    inline for (std.mem.zeroes([8]void)) |_, op| if (op + 0x08 == ix) {
        self.bit_shift(.right, .old_bit, repeated_registers[op % 8]);
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
        self.bit_shift(.right, .previous_bit, repeated_registers[op % 8]);
    };

    inline for (std.mem.zeroes([8]void)) |_, op| if (op + 0x30 == ix) {
        self.bit_swap(repeated_registers[op % 8]);
    };

    inline for (std.mem.zeroes([8]void)) |_, op| if (op + 0x38 == ix) {
        self.bit_shift(.right, .reset, repeated_registers[op % 8]);
    };

    inline for (std.mem.zeroes([8 * 8]void)) |_, op| if (op + 0x40 == ix) {
        const bit = @truncate(u3, op / 8);
        const arg = repeated_registers[op % 8];
        self.bit_test(bit, arg);
    };

    inline for (std.mem.zeroes([8 * 8]void)) |_, op| if (op + 0x80 == ix) {
        const bit = @truncate(u3, op / 8);
        const arg = repeated_registers[op % 8];
        self.bit_reset(bit, arg);
    };

    inline for (std.mem.zeroes([8 * 8]void)) |_, op| if (op + 0xC0 == ix) {
        const bit = @truncate(u3, op / 8);
        const arg = repeated_registers[op % 8];
        self.bit_set(bit, arg);
    };

    // Advance the program counter
    self.registers.pc +%= details.byte_length;

    return details.clock_cycles;
}

pub fn step_timers(self: *Self, clocks: usize) void {
    self.timer_div_clock += clocks;

    // TODO fix this.

    const one_frame = 456;

    if (self.timer_div_clock >= one_frame) {
        self.timer_div_clock %= one_frame;
        self.mmu.io_register(.timer_divider).* +%= 1;
    }

    if (!self.mmu.io_timer_control().enabled) {
        return;
    }

    self.timer_conf_clock += clocks;

    const timer_counter = self.mmu.io_register(.timer_counter);

    const increment_limit: usize = switch (self.mmu.io_timer_control().mode) {
        .fastest => one_frame / 16,
        .slowest => one_frame * 4,
        .fast => one_frame / 4,
        .slow => one_frame,
    };

    if (self.timer_conf_clock >= increment_limit) {
        self.timer_conf_clock %= increment_limit;

        if (timer_counter.* < 0xFF) {
            timer_counter.* +%= 1;
        } else {
            timer_counter.* = self.mmu.io_register(.timer_modulo).*;
            self.mmu.fire_interrupt(.timer);
        }
    }
}

pub fn step_interrupts(self: *Self) void {
    if (!self.registers.ime) {
        return; // All interrupts are disabled.
    }

    const irrpt_flag = self.mmu.io_interrupt_flag();
    const irrpt_enable = self.mmu.io_interrupt_enable();

    inline for (comptime std.enums.values(Interrupt)) |irrpt| {
        if (irrpt_enable.is_set(irrpt) and irrpt_flag.is_set(irrpt)) {
            // Disable all interrupts.
            self.interrupt_master_enable(false);

            // Set the interrupt as handled in the interrupt flags.
            self.mmu.io_interrupt_flag().set(irrpt, false);

            // Restart to interrupt handling address.
            self.restart(switch (irrpt) {
                .v_blank => .x40,
                .lcd_status => .x48,
                .timer => .x50,
                .serial => .x58,
                .joypad => .x60,
            });

            return;
        }
    }
}

pub fn step(self: *Self) usize {
    const clocks = self.step_instruction();
    self.step_timers(clocks);
    self.step_interrupts();
    return clocks;
}
