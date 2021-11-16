const std = @import("std");

const window = @import("window.zig");
const memory = @import("memory.zig");
const execution = @import("execution.zig");

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var win = try window.Window.init(&gpa.allocator, .{});
    defer win.deinit();

    var ram = try memory.Memory.init(&gpa.allocator);
    defer ram.deinit();

    // Load roms
    ram.load_boot_rom(memory.BootRom);
    // ram.load_game_rom(memory.PkmnBlue);
    ram.load_game_rom(@embedFile("roms/gb-test-roms/cpu_instrs/individual/06-ld r,r.gb")[0..]);

    var cpu = try execution.CPU.init(&ram, &gpa.allocator);
    defer cpu.deinit();

    const stdin = std.io.getStdIn().reader();

    while (win.alive()) {
        var draw = win.drawBegin();
        defer win.drawFinish();

        _ = cpu.step_instruction();

        if (ram.buffer[0xff02] == 0x81) {
            std.debug.print("{u}", .{ram.buffer[0xff01]});
            ram.buffer[0xff02] = 0x0;
        }

        // try stdin.skipUntilDelimiterOrEof('\n');
    }
}
