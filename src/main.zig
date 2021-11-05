const std = @import("std");

const memory = @import("memory.zig");
const window = @import("window.zig");
const execution = @import("execution.zig");

pub fn keyMapping(value: u8) window.Window.Key {
    return switch (value) {
        0x1 => .Num1,
        0x2 => .Num2,
        0x3 => .Num3,
        0xC => .Num4,

        0x4 => .Q,
        0x5 => .W,
        0x6 => .E,
        0xD => .R,

        0x7 => .A,
        0x8 => .S,
        0x9 => .D,
        0xE => .F,

        0xA => .Z,
        0x0 => .X,
        0xB => .C,
        0xF => .V,

        else => .Unknown,
    };
}

pub fn main() anyerror!void {
    var win = try window.Window.init(.{
        .width = 64,
        .height = 32,
    });
    defer win.deinit();

    var mem = try memory.Memory.init(std.heap.c_allocator);
    defer mem.deinit();

    var prng = std.rand.DefaultPrng.init(@intCast(u64, std.time.milliTimestamp()));

    const rom = @embedFile("./roms/pong.bin");
    mem.loadRom(std.mem.span(rom));

    while (win.alive()) : (mem.decrementTimers()) {
        var draw = win.drawBegin();
        defer win.drawFinish();

        for (std.mem.zeroes([10]void)) |_| {
            const opcode = execution.decodeInstruction(mem.fetchInstruction());

            switch (opcode.w) {
                // 0x1XYZ - Move program counter to XYZ
                0x1 => mem.jumpToAddress(opcode.xyz),

                // 0x2XYZ - Call subroutine starting at XYZ
                0x2 => mem.subroutineCall(opcode.xyz),

                // 0x3XYZ - Skips the next instruction if VX equals YZ
                0x3 => if (mem.variable[opcode.x] == opcode.yz) mem.skipInstruction(),

                // 0x4XYZ - Skips the next instruction if VX does not equal YZ
                0x4 => if (mem.variable[opcode.x] != opcode.yz) mem.skipInstruction(),

                // 0x5XY0 - Skips the next instruction if VX equals VY
                0x5 => if (mem.variable[opcode.x] == mem.variable[opcode.y]) mem.skipInstruction(),

                // 0x6XYZ - Set variable register VX to YZ
                0x6 => mem.variable[opcode.x] = opcode.yz,

                // 0x7XYZ - Add YZ to variable register VX
                0x7 => mem.addVariable(opcode.x, opcode.yz),

                // Bitwise operations
                0x8 => switch (opcode.z) {
                    // 0x8XY0 - Sets VX to the value of VY
                    0x0 => mem.variable[opcode.x] = mem.variable[opcode.y],

                    // 0x8XY1 - Sets VX to VX bitwise or VY
                    0x1 => mem.variable[opcode.x] |= mem.variable[opcode.y],

                    // 0x8XY2 - Sets VX to VX bitwise and VY
                    0x2 => mem.variable[opcode.x] &= mem.variable[opcode.y],

                    // 0x8XY3 - Sets VX to VX xor VY
                    0x3 => mem.variable[opcode.x] ^= mem.variable[opcode.y],

                    // 0x8XY4 - Adds VY to VX. VF is set to 1 when there's a carry, and to 0 when there is not.
                    0x4 => mem.addVariables(opcode.x, opcode.y),

                    // 0x8XY5 - VY is subtracted from VX. VF is set to 0 when there's a borrow, and 1 when there is not.
                    0x5 => mem.subVariables(opcode.x, opcode.y),

                    // 8XY6[a]	BitOp	Vx >>= 1	Stores the least significant bit of VX in VF and then shifts VX to the right by 1.[b]
                    // 8XY7[a]	Math	Vx = Vy - Vx	Sets VX to VY minus VX. VF is set to 0 when there's a borrow, and 1 when there is not.
                    // 8XYE[a]	BitOp	Vx <<= 1	Stores the most significant bit of VX in VF and then shifts VX to the left by 1.[b]
                    else => unreachable, // TODO
                },

                // 0xAXYZ - Set instruction register to XYZ
                0xA => mem.instruction = @intCast(u16, opcode.xyz),

                // 0xBXYZ - Jumps to the address XYZ plus V0
                0xB => mem.jumpToAddress(opcode.xyz + mem.variable[0x0]),

                // 0xCXYZ - Sets VX to YZ "bitwise anded" with a random value
                0xC => mem.variable[opcode.x] = prng.random.int(u8) & opcode.yz,

                // 0xDXYZ - Draw sprite at VX, VY with size 8xZ
                0xD => {
                    const x = mem.variable[opcode.x];
                    const y = mem.variable[opcode.y];

                    for (mem.buffer[mem.instruction..][0..opcode.z]) |sprite, offset| {
                        const yy = y + @intCast(u8, offset);
                        execution.drawSpriteRow(draw.screen, x, yy, sprite);
                    }
                },

                0xE => switch (opcode.yz) {
                    // 0xEX9E - Skips the next instruction if the key stored in VX is pressed
                    0x9E => if (win.keyPressed(keyMapping(mem.variable[opcode.x]))) mem.skipInstruction(),

                    // 0xEXA1 - Skips the next instruction if the key stored in VX is not pressed
                    0xA1 => if (!win.keyPressed(keyMapping(mem.variable[opcode.x]))) mem.skipInstruction(),

                    // Invalid opcode
                    else => unreachable, // TODO
                },

                0xF => switch (opcode.yz) {
                    // 0xFX07 - Set VX to value of delay timer
                    0x07 => mem.variable[opcode.x] = mem.delay_timer,

                    // 0xFX0A - Blocking for key press stored in VX
                    0x0A => unreachable, // TODO

                    // 0xFX15 - Sets the delay timer to VX
                    0x15 => mem.delay_timer = mem.variable[opcode.x],

                    // 0xFX18 - Sets the sound timer to VX
                    0x18 => mem.sound_timer = mem.variable[opcode.x],

                    // 0xFX1E - Adds VX to instruction
                    0x1E => mem.instruction += mem.variable[opcode.x],

                    // 0xFX29
                    0x29 => std.debug.print("Pula", .{}),

                    // 0xFX33 - Stores the binary-coded decimal of VX in instruction
                    0x33 => unreachable, // TODO

                    // 0xFX55 - Stores V0 to VX in memory starting at address instruction
                    0x55 => unreachable, // TODO

                    // 0xFX65 - Fills V0 to VX from memory starting at address instruction
                    0x65 => unreachable, // TODO

                    // Invalid opcode
                    else => unreachable, // TODO
                },

                // Misc. opcodes using the 0 prefix
                0x0 => switch (opcode.wxyz) {
                    0x00E0 => execution.clearScreen(draw.screen),
                    0x00EE => mem.subroutineReturn(),
                    else => mem.subroutineCall(opcode.xyz),
                },

                else => {
                    std.debug.print("UNKOWN OPCODE {x}\n", .{opcode.wxyz});
                    return error.UnkownOpcode;
                },
            }
        }
    }
}
