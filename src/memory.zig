const std = @import("std");

usingnamespace @import("instructions.zig");

const Self = @This();

const Size = 0xFFFF + 1;
const InterruptAddr = 0xFFFF;

const BootStart = 0x0000;
const BootEnd = 0x00FF;
const BootSize = 1 + BootEnd - BootStart;

const RomBank0Start = 0x0000;
const RomBank0End = 0x3FFF;
const RomBank0Size = 1 + RomBank0End - RomBank0Start;

const RomBankNStart = 0x4000;
const RomBankNEnd = 0x7FFF;
const RomBankNSize = 1 + RomBankNEnd - RomBankNStart;

const VideoStart = 0x8000;
const VideoEnd = 0x9FFF;
const VideoSize = 1 + VideoEnd - VideoStart;

const ExternStart = 0xA000;
const ExternEnd = 0xBFFF;
const ExternSize = 1 + ExternEnd - ExternStart;

const WorkStart = 0xC000;
const WorkEnd = 0xDFFF;
const WorkSize = 1 + WorkEnd - WorkStart;

const EchoStart = 0xE000;
const EchoEnd = 0xFDFF;
const EchoSize = 1 + EchoEnd - EchoStart;

const SprTableStart = 0xFE00;
const SprTableEnd = 0xFE9F;
const SprTableSize = 1 + SprTableEnd - SprTableStart;

const UnusedStart = 0xFEA0;
const UnusedEnd = 0xFEFF;

const IOStart = 0xFF00;
const IOEnd = 0xFF7F;
const IOSize = 1 + IOEnd - IOStart;

const HighStart = 0xFF80;
const HighEnd = 0xFFFF;
const HighSize = 1 + HighEnd - HighStart;

cartridge: []const u8,
allocator: *std.mem.Allocator,

bootloader: *const [BootSize]u8,
rom_bank_0: *const [RomBank0Size]u8,
rom_bank_n: *const [RomBankNSize]u8,

video_ram: *[VideoSize]u8,
extern_ram: *[ExternSize]u8,
work_ram: *[WorkSize]u8,
sprite_table: *[SprTableSize]u8,
input_output: *[IOSize]u8,
high_ram: *[HighSize]u8,

pub fn init(
    allocator: *std.mem.Allocator,
    bootrom: []const u8,
    cartridge: []const u8,
) !Self {
    std.debug.assert(bootrom.len == BootSize);
    std.debug.assert(cartridge.len >= RomBank0Size + RomBankNSize);

    const bootloader = try allocator.alloc(u8, BootSize);
    const rom_bank_0 = try allocator.alloc(u8, RomBank0Size);
    const rom_bank_n = try allocator.alloc(u8, RomBankNSize);

    std.mem.copy(u8, bootloader, bootrom);
    std.mem.copy(u8, rom_bank_0, cartridge[RomBank0Start..RomBank0End]);
    std.mem.copy(u8, rom_bank_n, cartridge[RomBankNStart..RomBankNEnd]);

    const video_ram = try allocator.alloc(u8, VideoSize);
    const extern_ram = try allocator.alloc(u8, ExternSize);
    const work_ram = try allocator.alloc(u8, WorkSize);
    const sprite_table = try allocator.alloc(u8, SprTableSize);
    const input_output = try allocator.alloc(u8, IOSize);
    const high_ram = try allocator.alloc(u8, HighSize);

    std.mem.set(u8, video_ram, 0);
    std.mem.set(u8, extern_ram, 0);
    std.mem.set(u8, sprite_table, 0);
    std.mem.set(u8, input_output, 0);
    std.mem.set(u8, high_ram, 0);

    return Self{
        .cartridge = cartridge,
        .allocator = allocator,

        .bootloader = bootloader[0..BootSize],
        .rom_bank_0 = rom_bank_0[0..RomBank0Size],
        .rom_bank_n = rom_bank_n[0..RomBankNSize],

        .video_ram = video_ram[0..VideoSize],
        .extern_ram = extern_ram[0..ExternSize],
        .work_ram = work_ram[0..WorkSize],
        .sprite_table = sprite_table[0..SprTableSize],
        .input_output = input_output[0..IOSize],
        .high_ram = high_ram[0..HighSize],
    };
}

pub fn deinit(self: Self) void {
    self.allocator.free(self.bootloader);
    self.allocator.free(self.rom_bank_0);
    self.allocator.free(self.rom_bank_n);

    self.allocator.free(self.video_ram);
    self.allocator.free(self.extern_ram);
    self.allocator.free(self.work_ram);
    self.allocator.free(self.sprite_table);
    self.allocator.free(self.input_output);
    self.allocator.free(self.high_ram);
}

fn map_bank_0(self: *Self, addr: u16) *const u8 {
    if (addr < 0x100 and self.io_register(.disable_boot).* == 0) {
        return &self.bootloader[addr];
    } else {
        return &self.rom_bank_0[addr];
    }
}

fn map_bank_n(self: *Self, addr: u16) *const u8 {
    // TODO Map switchable memory bank.
    return &self.rom_bank_n[addr];
}

fn map_write_address(self: *Self, addr: u16) *u8 {
    return switch (addr) {
        VideoStart...VideoEnd => &self.video_ram[addr - VideoStart],
        ExternStart...ExternEnd => &self.extern_ram[addr - ExternStart],
        WorkStart...WorkEnd => &self.work_ram[addr - WorkStart],
        EchoStart...EchoEnd => &self.work_ram[addr - EchoStart],
        SprTableStart...SprTableEnd => &self.sprite_table[addr - SprTableStart],
        UnusedStart...UnusedEnd => &self.work_ram[addr - UnusedStart],
        IOStart...IOEnd => &self.input_output[addr - IOStart],
        HighStart...HighEnd => &self.high_ram[addr - HighStart],
        else => std.debug.panic("write to readonly address {X:0>4}\n", .{addr}),
    };
}

fn map_read_address(self: *Self, addr: u16) *const u8 {
    return switch (addr) {
        RomBank0Start...RomBank0End => self.map_bank_0(addr - RomBank0Start),
        RomBankNStart...RomBankNEnd => self.map_bank_n(addr - RomBankNStart),
        else => self.map_write_address(addr),
    };
}

fn is_readonly(addr: u16) bool {
    return switch (addr) {
        RomBank0Start...RomBank0End => true,
        RomBankNStart...RomBankNEnd => true,
        EchoStart...EchoEnd => true,
        UnusedStart...UnusedEnd => true,
        else => false,
    };
}

pub fn read(self: *Self, comptime t: type, addr: u16) t {
    if (@sizeOf(t) == 1) {
        return @ptrCast(*const t, self.map_read_address(addr)).*;
    }

    var bytes: [@sizeOf(t)]u8 = undefined;

    for (bytes) |*byte, offset| {
        byte.* = self.map_read_address(addr +% @truncate(u16, offset)).*;
    }

    return std.mem.bytesToValue(t, &bytes);
}

pub fn write(self: *Self, comptime t: type, start_addr: u16, value: t) void {
    for (std.mem.toBytes(value)) |byte, offset| {
        const addr = start_addr +% @truncate(u16, offset);

        if (!is_readonly(addr)) {
            self.map_write_address(addr).* = byte;
        }
    }
}

pub const IORegister = enum(u16) {
    controller = 0xFF00,
    timer_divider = 0xFF04,
    timer_counter = 0xFF05,
    timer_modulo = 0xFF06,
    timer_control = 0xFF07,
    interrupt_flag = 0xFF0F,
    lcd_control = 0xFF40,
    lcd_status = 0xFF41,
    scroll_y = 0xFF42,
    scroll_x = 0xFF43,
    lcd_y_coord = 0xFF44,
    lcd_y_compare = 0xFF45,
    dma_transfer = 0xFF46,
    bg_palette = 0xFF47,
    spr_palette_0 = 0xFF48,
    spr_palette_1 = 0xFF49,
    window_y = 0xFF4A,
    window_x = 0xFF4B,
    disable_boot = 0xFF50,
    interrupt_enable = 0xFFFF,
};

pub fn io_register(self: *Self, comptime reg: IORegister) *u8 {
    return self.map_write_address(@enumToInt(reg));
}

pub fn io_controller(self: *Self) *Controller {
    return @ptrCast(*Controller, self.io_register(.controller));
}

pub fn io_timer_control(self: *Self) *TimerControl {
    return @ptrCast(*TimerControl, self.io_register(.timer_control));
}

pub fn io_interrupt_flag(self: *Self) *InterruptFlags {
    return @ptrCast(*InterruptFlags, self.io_register(.interrupt_flag));
}

pub fn io_lcd_control(self: *Self) *LCDControl {
    return @ptrCast(*LCDControl, self.io_register(.lcd_control));
}

pub fn io_lcd_status(self: *Self) *LCDStatus {
    return @ptrCast(*LCDStatus, self.io_register(.lcd_status));
}

pub fn io_interrupt_enable(self: *Self) *InterruptFlags {
    return @ptrCast(*InterruptFlags, self.io_register(.interrupt_enable));
}

pub fn fire_interrupt(self: *Self, comptime irrpt: Interrupt) void {
    if (self.io_interrupt_enable().is_set(irrpt)) {
        self.io_interrupt_flag().set(irrpt, true);
    }
}

pub fn gameTitle(self: *Self) []const u8 {
    const title = self.cartridge[0x0134..0x0143];
    for (title) |char| std.debug.assert(std.ascii.isASCII(char));
    return title;
}
