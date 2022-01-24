const std = @import("std");

const MMU = @import("memory.zig");

usingnamespace @import("instructions.zig");

const Self = @This();

const TileSide: u4 = 8;

const TileMapAddrs = [2]u16{
    0x9800, // 0 = 9800 -> 9BFF
    0x9C00, // 1 = 9C00 -> 9FFF
};

const Sprite = packed struct {
    y_position: u8,
    x_position: u8,
    tile_index: u8,
    attributes: packed struct {
        unused: u4,
        palette: u1,
        x_flip: u1,
        y_flip: u1,
        priority: u1,
    },
};

const SpriteLineLimit: usize = 10;
const SpriteTableStart: u16 = 0xFE00;
const SpriteTableEnd: u16 = 0xFE9F;
const SpriteTableCapacity: usize = 40;

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
allocator: std.mem.Allocator,
framebuffer: *[FramebufferSize]u8,

line: u8,
win_line: usize,
dots: usize,
ready: bool,
state: PPUState,

pub fn init(mmu: *MMU, allocator: std.mem.Allocator) !Self {
    const framebuffer = try allocator.alloc(u8, FramebufferSize);

    return Self{
        .mmu = mmu,
        .allocator = allocator,
        .framebuffer = framebuffer[0..FramebufferSize],

        .line = 0,
        .dots = 0,
        .win_line = 0,
        .ready = false,
        .state = .h_blank,
    };
}

pub fn deinit(self: *Self) void {
    self.allocator.free(self.framebuffer);
}

fn pixelColor(palette: u8, index: u2) u8 {
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

fn readTilePixel(self: *Self, id: u8, x: u4, y: u4, mode: ReadTileMode) u2 {
    const addr = switch (mode) {
        .block_0 => 0x8000 + 16 * @intCast(u16, id),
        .block_1 => 0x8800 + 16 * @intCast(u16, id +% 0x80),
    };

    const row = self.mmu.read(u16, addr + 2 * @intCast(u16, y));

    const bit1 = @truncate(u2, row >> (14 - x)) & 0b10;
    const bit2 = @truncate(u2, row >> (07 - x)) & 0b01;

    return bit1 | bit2;
}

fn drawBackgroundPixelLine(self: *Self) void {
    const lcd_control = self.mmu.ioLCDControl();

    const scx = self.mmu.ioRegister(.scroll_x).*;
    const scy = self.mmu.ioRegister(.scroll_y).*;

    const palette = self.mmu.ioRegister(.bg_palette).*;
    const tilemap = TileMapAddrs[lcd_control.background_tilemap];

    var pixel_x: u8 = 0;

    while (pixel_x < FramebufferWidth) : (pixel_x += 1) {
        const x = scx +% pixel_x;
        const y = scy +% self.line;

        const id = self.mmu.read(u8, tilemap + @intCast(u16, y / 8) * 32 + (x / 8));

        const tile_x = @intCast(u4, x % 8);
        const tile_y = @intCast(u4, y % 8);

        const pixel = self.readTilePixel(id, tile_x, tile_y, switch (lcd_control.bg_win_tiledata) {
            0 => .block_1,
            1 => .block_0,
        });

        const fb_index = @intCast(usize, self.line) * FramebufferWidth + pixel_x;
        self.framebuffer[fb_index] = pixelColor(palette, pixel);
    }
}

fn drawWindowPixelLine(self: *Self) void {
    const lcd_control = self.mmu.ioLCDControl();

    const wx = self.mmu.ioRegister(.window_x).*;
    const wy = self.mmu.ioRegister(.window_y).*;

    if (self.line < wy) return;
    if (FramebufferWidth - 7 < wx) return;

    const palette = self.mmu.ioRegister(.bg_palette).*;
    const tilemap = TileMapAddrs[lcd_control.window_tilemap];

    const line_y = self.win_line;
    self.win_line += 1;

    var pixel_x: u8 = if (wx > 7) wx - 7 else 0;

    while (pixel_x < FramebufferWidth) : (pixel_x += 1) {
        const x = pixel_x + 7 - wx;
        const y = line_y;

        const id = self.mmu.read(u8, tilemap + @intCast(u16, y / 8) * 32 + (x / 8));

        const tile_x = @intCast(u4, x % 8);
        const tile_y = @intCast(u4, y % 8);

        const pixel = self.readTilePixel(id, tile_x, tile_y, switch (lcd_control.bg_win_tiledata) {
            0 => .block_1,
            1 => .block_0,
        });

        const fb_index = @intCast(usize, self.line) * FramebufferWidth + pixel_x;
        self.framebuffer[fb_index] = pixelColor(palette, pixel);
    }
}

fn readSprite(self: *Self, id: u8) Sprite {
    return self.mmu.read(Sprite, SpriteTableStart + @sizeOf(Sprite) * id);
}

fn drawSpritesPixelLine(self: *Self) void {
    const lcd_control = self.mmu.ioLCDControl();

    const actual_line: usize = self.line + 16;
    const sprite_height: u8 = if (lcd_control.sprites_tall) 16 else 8;

    // FIXME drawing tall sprites is broken.
    if (lcd_control.sprites_tall) return;

    var sprite_id: u8 = 0;
    var sprites_drawn: usize = 0;

    while (sprite_id < SpriteTableCapacity) : (sprite_id += 1) {
        // Do not draw more than 10 sprites per line.
        if (sprites_drawn >= SpriteLineLimit) break;

        const sprite = self.readSprite(sprite_id);

        if (sprite.y_position > actual_line) continue;
        if (sprite.y_position + sprite_height <= actual_line) continue;

        sprites_drawn += 1;

        const palette = switch (sprite.attributes.palette) {
            0 => self.mmu.ioRegister(.spr_palette_0).*,
            1 => self.mmu.ioRegister(.spr_palette_1).*,
        };

        const spry = @truncate(u4, actual_line - sprite.y_position);
        var sprx = @intCast(u4, std.math.max(0, TileSide - @intCast(i16, sprite.x_position)));

        while (sprx < 8) : (sprx += 1) {
            const px = if (sprite.attributes.x_flip == 1) TileSide - 1 - sprx else sprx;
            const py = if (sprite.attributes.y_flip == 1) TileSide - 1 - spry else spry;

            const pixel = self.readTilePixel(sprite.tile_index, px, py, .block_0);
            const pixel_x = sprite.x_position + sprx - 8;

            const fb_index = @intCast(usize, self.line) * FramebufferWidth + pixel_x;
            self.framebuffer[fb_index] = pixelColor(palette, pixel);
        }
    }
}

fn drawPixelLine(self: *Self) void {
    const lcd_control = self.mmu.ioLCDControl();

    if (lcd_control.bg_win_enable) {
        self.drawBackgroundPixelLine();
    }

    if (lcd_control.bg_win_enable and lcd_control.window_enable) {
        self.drawWindowPixelLine();
    }

    if (lcd_control.sprites_enable) {
        self.drawSpritesPixelLine();
    }
}

fn lineNext(self: *Self) void {
    self.line +%= 1;

    const lyc = self.mmu.ioRegister(.lcd_y_compare).*;

    self.mmu.ioRegister(.lcd_y_coord).* = self.line;
    self.mmu.ioLCDStatus().lyc_equal_ly = self.line == lyc;

    if (self.line == lyc and self.mmu.ioLCDStatus().lyc_interrupt) {
        self.mmu.fireInterrupt(.lcd_status);
    }
}

fn finishFrame(self: *Self) void {
    self.ready = true;
    self.line = 0xFF;
    self.win_line = 0x0;
    self.lineNext();
}

fn updateState(self: *Self, comptime new_state: PPUState) void {
    self.state = new_state;

    self.mmu.ioLCDStatus().draw_stage = switch (new_state) {
        .v_blank => .v_blank,
        .h_blank => .h_blank,
        .oam_scan => .oam_scan,
        .pixel_transfer => .pixel_transfer,
    };

    switch (new_state) {
        .v_blank => if (self.mmu.ioLCDStatus().vblank_interrupt) {
            self.mmu.fireInterrupt(.lcd_status);
        },

        .h_blank => if (self.mmu.ioLCDStatus().hblank_interrupt) {
            self.mmu.fireInterrupt(.lcd_status);
        },

        .oam_scan => if (self.mmu.ioLCDStatus().oam_interrupt) {
            self.mmu.fireInterrupt(.lcd_status);
        },

        else => {},
    }
}

pub fn step(self: *Self, clocks: usize) bool {
    // Step CPU continously while PPU is off.
    if (!self.mmu.ioLCDControl().lcd_ppu_enable) {
        return true;
    }

    self.dots += clocks;

    switch (self.state) {
        .h_blank => if (self.dots >= HBlankDots) {
            self.dots %= HBlankDots;

            if (self.line == VBlankLine) {
                self.updateState(.v_blank);
            } else {
                self.updateState(.oam_scan);
                self.lineNext();
            }
        },

        .v_blank => if (self.dots >= VBlankDots) {
            self.dots %= VBlankDots;

            if (self.line == RenderLine) {
                self.updateState(.oam_scan);
                self.finishFrame();
            } else {
                self.lineNext();
            }
        },

        .oam_scan => if (self.dots >= OAMScanDots) {
            self.dots %= OAMScanDots;
            self.updateState(.pixel_transfer);
        },

        .pixel_transfer => if (self.dots >= PixelTransferDots) {
            self.dots %= PixelTransferDots;
            self.drawPixelLine();
            self.updateState(.h_blank);
        },
    }

    return !self.ready;
}

pub fn blit(self: *Self, screen: []u8) void {
    std.debug.assert(screen.len == FramebufferSize);
    std.mem.copy(u8, screen, self.framebuffer);
    self.ready = false;
}
