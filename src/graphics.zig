const std = @import("std");

const MMU = @import("memory.zig");

usingnamespace @import("instructions.zig");

const Self = @This();

const TileMapAddrs = [2]u16{
    0x9800, // 0 = 9800 -> 9BFF
    0x9C00, // 1 = 9C00 -> 9FFF
};

const Sprite = packed struct {
    attributes: packed struct {
        unused: u4,
        palette: u1,
        x_flip: u1,
        y_flip: u1,
        priority: u1,
    },

    tile_index: u8,
    x_position: u8,
    y_position: u8,
};

const SpriteTableStart: u16 = 0xFE00;
const SpriteTableEnd: u16 = 0xFE9F;

const PPUState = enum {
    h_blank,
    v_blank,
    oam_scan,
    pixel_transfer,
};

const HBlankDots = 204;
const VBlankDots = 456;
const OAMScanDots = 80;
const PixelTransferDots = 172;

const VBlankLine = 143;
const RenderLine = 153;

const FramebufferWidth = 160;
const FramebufferHeight = 144;
const FramebufferSize = FramebufferWidth * FramebufferHeight;

mmu: *MMU,
allocator: *std.mem.Allocator,
framebuffer: *[FramebufferSize]u8,

line: u8,
dots: usize,
ready: bool,
state: PPUState,

pub fn init(mmu: *MMU, allocator: *std.mem.Allocator) !Self {
    const framebuffer = try allocator.alloc(u8, FramebufferSize);

    return Self{
        .mmu = mmu,
        .allocator = allocator,
        .framebuffer = framebuffer[0..FramebufferSize],

        .line = 0,
        .dots = 0,
        .ready = false,
        .state = .h_blank,
    };
}

pub fn deinit(self: *Self) void {
    self.allocator.free(self.framebuffer);
}

fn pixel_color(palette: u8, index: u2) u8 {
    const PixelWhite = 0xFF;
    const PixelLight = 0x80;
    const PixelDark = 0x40;
    const PixelBlack = 0x16;

    const shift = 2 * @intCast(u3, index);

    return switch (@truncate(u2, palette >> shift)) {
        0 => PixelWhite,
        1 => PixelLight,
        2 => PixelDark,
        3 => PixelBlack,
    };
}

const ReadTileMode = enum { block_0, block_1 };

fn read_tile_pixel(self: *Self, id: u8, x: u4, y: u4, mode: ReadTileMode) u2 {
    const addr = switch (mode) {
        .block_0 => 0x8000 + 16 * @intCast(u16, id),
        .block_1 => 0x8800 + 16 * @intCast(u16, id +% 0x80),
    };

    const row = self.mmu.read(u16, addr + 2 * @intCast(u16, y));

    const bit1 = @truncate(u2, row >> (14 - x)) & 0b10;
    const bit2 = @truncate(u2, row >> (07 - x)) & 0b01;

    return bit1 | bit2;
}

fn draw_background_pixel_line(self: *Self) void {
    const lcd_control = self.mmu.io_lcd_control();

    const scx = self.mmu.io_register(.scroll_x).*;
    const scy = self.mmu.io_register(.scroll_y).*;

    const palette = self.mmu.io_register(.bg_palette).*;
    const tilemap = TileMapAddrs[lcd_control.background_tilemap];

    var pixel_x: u8 = 0;

    while (pixel_x < FramebufferWidth) : (pixel_x += 1) {
        const x = scx +% pixel_x;
        const y = scy +% self.line;

        const id = self.mmu.read(u8, tilemap + @intCast(u16, y / 8) * 32 + (x / 8));

        const tile_x = @intCast(u4, x % 8);
        const tile_y = @intCast(u4, y % 8);

        const pixel = self.read_tile_pixel(id, tile_x, tile_y, switch (lcd_control.bg_win_tiledata) {
            0 => .block_1,
            1 => .block_0,
        });

        const fb_index = @intCast(usize, self.line) * FramebufferWidth + pixel_x;
        self.framebuffer[fb_index] = pixel_color(palette, pixel);
    }
}

fn draw_window_pixel_line(self: *Self) void {
    const wx = self.mmu.io_register(.window_x).*;
    const wy = self.mmu.io_register(.window_y).*;

    // TODO implement drawing of window layer.
}

fn read_sprite(self: *Self, id: u8) Sprite {
    return self.mmu.read(Sprite, SpriteTableStart + id);
}

fn draw_sprites_pixel_line(self: *Self) void {
    // TODO implement drawing of sprites.
}

fn draw_pixel_line(self: *Self) void {
    const lcd_control = self.mmu.io_lcd_control();

    if (lcd_control.bg_win_enable) {
        self.draw_background_pixel_line();
    }

    if (lcd_control.bg_win_enable and lcd_control.window_enable) {
        self.draw_window_pixel_line();
    }

    if (lcd_control.sprites_enable) {
        self.draw_sprites_pixel_line();
    }
}

fn line_next(self: *Self) void {
    self.line +%= 1;

    const lyc = self.mmu.io_register(.lcd_y_compare).*;

    self.mmu.io_register(.lcd_y_coord).* = self.line;
    self.mmu.io_lcd_status().lyc_equal_ly = self.line == lyc;

    if (self.line == lyc and self.mmu.io_lcd_status().lyc_interrupt) {
        self.mmu.fire_interrupt(.lcd_status);
    }
}

fn line_reset(self: *Self) void {
    self.line = 0xFF;
    self.ready = true;
    self.line_next();
}

fn update_state(self: *Self, comptime new_state: PPUState) void {
    self.state = new_state;

    self.mmu.io_lcd_status().draw_stage = switch (new_state) {
        .v_blank => .v_blank,
        .h_blank => .h_blank,
        .oam_scan => .oam_scan,
        .pixel_transfer => .pixel_transfer,
    };

    switch (new_state) {
        .v_blank => if (self.mmu.io_lcd_status().vblank_interrupt) {
            self.mmu.fire_interrupt(.lcd_status);
        },

        .h_blank => if (self.mmu.io_lcd_status().hblank_interrupt) {
            self.mmu.fire_interrupt(.lcd_status);
        },

        .oam_scan => if (self.mmu.io_lcd_status().oam_interrupt) {
            self.mmu.fire_interrupt(.lcd_status);
        },

        else => {},
    }
}

pub fn step(self: *Self, clocks: usize) bool {
    // Step CPU continously while PPU is off.
    if (!self.mmu.io_lcd_control().lcd_ppu_enable) {
        return true;
    }

    self.dots += clocks;

    switch (self.state) {
        .h_blank => if (self.dots >= HBlankDots) {
            self.dots %= HBlankDots;

            if (self.line == VBlankLine) {
                self.update_state(.v_blank);
            } else {
                self.update_state(.oam_scan);
                self.line_next();
            }
        },

        .v_blank => if (self.dots >= VBlankDots) {
            self.dots %= VBlankDots;

            if (self.line == RenderLine) {
                self.update_state(.oam_scan);
                self.line_reset();
            } else {
                self.line_next();
            }
        },

        .oam_scan => if (self.dots >= OAMScanDots) {
            self.dots %= OAMScanDots;
            self.update_state(.pixel_transfer);
        },

        .pixel_transfer => if (self.dots >= PixelTransferDots) {
            self.dots %= PixelTransferDots;
            self.draw_pixel_line();
            self.update_state(.h_blank);
        },
    }

    return !self.ready;
}

pub fn blit(self: *Self, screen: []u8) void {
    std.debug.assert(screen.len == FramebufferSize);
    std.mem.copy(u8, screen, self.framebuffer);
    self.ready = false;
}
