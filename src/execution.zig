const std = @import("std");

pub const Instruction = struct {
    w: u4,
    x: u4,
    y: u4,
    z: u4,
    yz: u8,
    xyz: u12,
    wxyz: u16,
};

pub fn decodeInstruction(instruction: u16) Instruction {
    return .{
        .w = @intCast(u4, (instruction >> 12) & 0xf),
        .x = @intCast(u4, (instruction >> 08) & 0xf),
        .y = @intCast(u4, (instruction >> 04) & 0xf),
        .z = @intCast(u4, (instruction >> 00) & 0xf),

        .yz = @intCast(u8, instruction & 0x00ff),
        .xyz = @intCast(u12, instruction & 0x0fff),
        .wxyz = instruction,
    };
}

pub fn clearScreen(screen: []u8) void {
    for (screen) |*pixel| pixel.* = 0;
}


pub fn drawSpriteRow(screen: []u8, x: u8, y: u8, sprite: u8) void {
    // std.debug.print("drawing at {d},{d}:\t{b}\n", .{x, y, sprite});
    // return;
    // TODO: no, no, no just eww
    // TODO: rewrite this whole thing
    // TODO: URGENTLY

    var dx: u4 = 0;

    while (dx < 8) : (dx += 1) {
        const mask = @as(u16, 0b1000_0000) >> dx;

        if ((sprite & mask) != 0) {
            // std.debug.print("pixel at {d},{d}", .{ x+dx, y });
            const idx = (@intCast(usize, y)) * 64 + @intCast(usize, x) + dx;
            if (idx < screen.len) screen[idx] ^= 255;
        }
    }
}


test "decoding instructions" {
    try std.testing.expectEqual(
        [_]u4{ 0xe, 0xb, 0xa, 0x3 },
        decodeInstruction(0xeba3),
    );
}
