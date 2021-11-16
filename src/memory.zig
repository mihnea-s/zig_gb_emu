const std = @import("std");

pub const BootRom = @embedFile("roms/dmg_boot.bin")[0..];

pub const PkmnBlue = @embedFile("roms/pkmn_blue.gb")[0..];

pub const Tile = [16]u8;

pub const Memory = struct {
    const Self = @This();

    const MemorySize = 0xFFFF + 1;
    const InterruptAddr = 0xFFFF;

    const RomStart = 0x0000;
    const RomEnd = 0x3FFF;
    const RomSize = RomEnd - RomStart;

    const VideoStart = 0x8000;
    const VideoEnd = 0x9FFF;
    const VideoSize = VideoEnd - VideoStart;

    const SprTableStart = 0xFE00;
    const SprTableEnd = 0xFE9F;
    const SprTableSize = SprTableEnd - SprTableStart;

    const IOStart = 0xFF00;
    const IOEnd = 0xFF7F;
    const IOSize = IOEnd - IOStart;

    const HighStart = 0xFF80;
    const HighEnd = 0xFFFE;
    const HighSize = HighEnd - HighStart;

    buffer: *[MemorySize]u8,
    interrupts: *u8,
    allocator: *std.mem.Allocator,

    rom: *[RomSize]u8,
    video: *[VideoSize]u8,
    sprite_table: *[SprTableSize]u8,
    input_output: *[IOSize]u8,
    high: *[HighSize]u8,

    pub fn init(allocator: *std.mem.Allocator) !Self {
        const buffer = try allocator.alloc(u8, MemorySize);

        // Zero initialise memory buffer
        for (buffer) |*buf_byte| buf_byte.* = 0;

        // Set interrupt register pointer
        const interrupts = &buffer[InterruptAddr];

        return Self{
            .buffer = buffer[0..MemorySize],
            .allocator = allocator,
            .interrupts = interrupts,

            .rom = buffer[RomStart..RomEnd],
            .video = buffer[VideoStart..VideoEnd],
            .sprite_table = buffer[SprTableStart..SprTableEnd],
            .input_output = buffer[IOStart..IOEnd],
            .high = buffer[HighStart..HighEnd],
        };
    }

    pub fn deinit(self: Self) void {
        self.allocator.free(self.buffer);
    }

    pub fn read(self: *Self, comptime t: type, addr: u16) t {
        return std.mem.bytesToValue(t, self.buffer[addr..][0..@sizeOf(t)]);
    }

    pub fn read_ptr(self: *Self, comptime t: type, addr: u16) *align(1) t {
        return std.mem.bytesAsValue(t, self.buffer[addr..][0..@sizeOf(t)]);
    }

    pub fn write(self: *Self, comptime t: type, addr: u16, value: t) void {
        std.mem.copy(u8, self.buffer[addr..][0..@sizeOf(t)], std.mem.toBytes(value)[0..]);
    }

    pub fn load_boot_rom(self: *Self, rom: []const u8) void {
        std.mem.copy(u8, self.buffer, rom[0x00..0xFF]);
    }

    pub fn load_game_rom(self: *Self, rom: []const u8) void {
        std.mem.copy(u8, self.buffer[0x100..], rom[0..RomSize]);
    }
};
