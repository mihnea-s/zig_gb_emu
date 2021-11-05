const std = @import("std");

pub const system_font = [_]u8{
    0xF0, 0x90, 0x90, 0x90, 0xF0, // 0
    0x20, 0x60, 0x20, 0x20, 0x70, // 1
    0xF0, 0x10, 0xF0, 0x80, 0xF0, // 2
    0xF0, 0x10, 0xF0, 0x10, 0xF0, // 3
    0x90, 0x90, 0xF0, 0x10, 0x10, // 4
    0xF0, 0x80, 0xF0, 0x10, 0xF0, // 5
    0xF0, 0x80, 0xF0, 0x90, 0xF0, // 6
    0xF0, 0x10, 0x20, 0x40, 0x40, // 7
    0xF0, 0x90, 0xF0, 0x90, 0xF0, // 8
    0xF0, 0x90, 0xF0, 0x10, 0xF0, // 9
    0xF0, 0x90, 0xF0, 0x90, 0x90, // A
    0xE0, 0x90, 0xE0, 0x90, 0xE0, // B
    0xF0, 0x80, 0x80, 0x80, 0xF0, // C
    0xE0, 0x90, 0x90, 0x90, 0xE0, // D
    0xF0, 0x80, 0xF0, 0x80, 0xF0, // E
    0xF0, 0x80, 0xF0, 0x80, 0x80, // F
};

pub const Memory = struct {
    const Self = @This();

    const MemorySize = 4096;
    const VariablesSize = 0x10;

    const StackSize = 0x30;
    const StackCapacity = StackSize / 2;

    const StackStart = 0x00;
    const StackEnd = StackStart + StackSize;

    const VariablesStart = StackEnd;
    const VariablesEnd = VariablesStart + VariablesSize;

    const FontStart = VariablesEnd;
    const FontEnd = FontStart + system_font.len;

    // Games are loaded at 0x200
    const RomStart = 0x200;

    instruction: u16,
    program_counter: u16,

    // Other Registers
    delay_timer: u8,
    sound_timer: u8,
    variable: *[VariablesSize]u8,

    // Stack pointer into memory
    stack_size: u8,
    stack: *align(1) [StackCapacity]u16,

    // Buffer to hold all memory
    buffer: *[MemorySize]u8,
    allocator: *std.mem.Allocator,

    pub fn init(allocator: *std.mem.Allocator) !Self {
        const buffer = try allocator.alloc(u8, MemorySize);

        const stack = std.mem.bytesAsSlice(
            u16,
            buffer[Memory.StackStart..Memory.StackEnd],
        )[0..StackCapacity];

        const variable = buffer[Memory.VariablesStart..Memory.VariablesEnd];

        std.mem.copy(u8, buffer[FontStart..FontEnd], &system_font);

        return Self{
            .instruction = 0x00,
            .program_counter = 0x00,

            .delay_timer = 0x00,
            .sound_timer = 0x00,
            .variable = variable,

            .stack = stack,
            .stack_size = 0,

            .buffer = buffer[0..MemorySize],
            .allocator = allocator,
        };
    }

    pub fn deinit(self: Self) void {
        self.allocator.free(self.buffer);
    }

    pub fn stackPush(self: *Self, value: u16) !void {
        if (self.stack_size >= StackCapacity) {
            return error.StackOverflow;
        }

        self.stack[self.stack_size] = value;
        self.stack_size += 1;
    }

    pub fn stackPop(self: *Self) !u16 {
        if (self.stack_size == 0) {
            return error.StackUnderflow;
        }

        self.stack_size -= 1;
        return self.stack[self.stack_size];
    }

    pub fn loadRom(self: *Self, rom: []const u8) void {
        std.mem.copy(u8, self.buffer[RomStart..], rom);
        self.program_counter = RomStart;
    }

    pub fn decrementTimers(self: *Self) void {
        if (self.delay_timer > 0) self.delay_timer -= 1;
        if (self.sound_timer > 0) self.sound_timer -= 1;
    }

    pub fn fetchInstruction(self: *Self) u16 {
        defer self.program_counter += 2;
        return std.mem.readIntBig(u16, self.buffer[self.program_counter..][0..2]);
    }

    pub fn skipInstruction(self: *Self) void {
        self.program_counter += 2;
    }

    pub fn jumpToAddress(self: *Self, address: u12) void {
        self.program_counter = @intCast(u16, address);
    }

    pub fn subroutineCall(self: *Self, address: u12) void {
        // TODO implement calling subroutines
    }

    pub fn subroutineReturn(self: *Self) void {
        // TODO implement returning from subroutines
    }

    pub fn addVariable(self: *Self, x: u4, value: u8) void {
        _ = @addWithOverflow(u8, self.variable[x], value, &self.variable[x]);
    }

    pub fn addVariables(self: *Self, x: u4, y: u4) void {
        const vx = &self.variable[x];
        const vy = &self.variable[y];

        if (@addWithOverflow(u8, vx.*, vy.*, vx)) {
            self.variable[0xf] = 1;
        } else {
            self.variable[0xf] = 0;
        }
    }

    pub fn subVariables(self: *Self, x: u4, y: u4) void {
        const vx = &self.variable[x];
        const vy = &self.variable[y];

        if (@subWithOverflow(u8, vx.*, vy.*, vx)) {
            self.variable[0xf] = 0;
        } else {
            self.variable[0xf] = 1;
        }
    }
};

test "system font loaded" {
    var memory = try Memory.init(std.testing.allocator);
    defer memory.deinit();

    const memory_font = memory.buffer[Memory.FontStart..Memory.FontEnd];
    try std.testing.expectEqualSlices(u8, &system_font, memory_font);
}

test "stack overflow & underflow" {
    var memory = try Memory.init(std.testing.allocator);
    defer memory.deinit();

    inline for (std.mem.zeroes([Memory.StackCapacity]void)) |_| {
        try memory.stackPush(0xaaff);
    }

    try std.testing.expectError(error.StackOverflow, memory.stackPush(0xbaad));

    inline for (std.mem.zeroes([Memory.StackCapacity]void)) |_| {
        _ = try memory.stackPop();
    }

    try std.testing.expectError(error.StackUnderflow, memory.stackPop());
}

test "fetch & skip instruction" {}
