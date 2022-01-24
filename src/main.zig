const std = @import("std");

const window = @import("window.zig");

const MMU = @import("memory.zig");
const PPU = @import("graphics.zig");
const CPU = @import("execution.zig");

const Debugger = @import("debugger.zig");

const BootRom = @embedFile("roms/boot.bin");
const GameRom = @embedFile("roms/game.bin");

// TODO:
// - Fix timers
// - Sprite drawing
// - Implement joypad
// - Fix ADC, SBC instructions
// - Fix SP + i8 instructions
// - Implement MBC/ERam control

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var mmu = try MMU.init(&gpa.allocator, BootRom, GameRom);
    defer mmu.deinit();

    var ppu = try PPU.init(&mmu, &gpa.allocator);
    defer ppu.deinit();

    var cpu = try CPU.init(&mmu, &gpa.allocator);
    defer cpu.deinit();

    if (std.os.argv.len > 1) {
        try Debugger.init(&gpa.allocator, &mmu, &ppu, &cpu).debug();
        return;
    }

    var win = try window.Window.init(&gpa.allocator, .{});
    defer win.deinit();

    // Set the game title to the window title.
    try win.setTitle(mmu.gameTitle());

    while (win.alive()) {
        const t_a = std.time.nanoTimestamp();

        var draw = win.drawBegin();
        defer win.drawFinish();

        // Step until the PPU can draw a frame.
        while (ppu.step(cpu.step())) {}

        ppu.blit(draw.screen);

        const t_b = std.time.nanoTimestamp();
        std.debug.print("Δt = {e:.6}, pc = ${X:0>4}\n", .{
            @intToFloat(f64, t_b - t_a) / 1_000_000,
            cpu.registers.pc,
        });
    }
}
