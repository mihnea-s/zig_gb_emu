const std = @import("std");

pub const FlagRegister = packed struct {
    zero: u1 = 0, // set if last operation resulted in zero, or CMP'd values matched
    nsub: u1 = 0, // set if subtraction occured in the last operation
    half: u1 = 0, // set if a carry occured in the lower nibble of the last operation
    cary: u1 = 0, // set if a carry occurde or A is smaller in the CP instruction
    rest: u4 = 0, // lower nibble of the flag register
};

pub const Registers = struct {
    sp: u16 = 0, // 16-bit stack pointer

    pc: u16 = 0, // 16-bit program counter

    ie: bool = true, // interrupts enabled

    af: packed union {
        all: u16,
        ind: packed struct {
            a: u8,
            f: FlagRegister,
        },
    } = .{ .all = 0 },

    bc: packed union {
        all: u16,
        ind: packed struct {
            b: u8,
            c: u8,
        },
    } = .{ .all = 0 },

    de: packed union {
        all: u16,
        ind: packed struct {
            d: u8,
            e: u8,
        },
    } = .{ .all = 0 },

    hl: packed union {
        all: u16,
        ind: packed struct {
            h: u8,
            l: u8,
        },
    } = .{ .all = 0 },
};

pub const Instruction = struct {
    mnemonic: []const u8,
    byte_length: u16,
    clock_cycles: usize,
};

fn instruction(
    mnemonic: []const u8,
    byte_length: u16,
    clock_cycles: usize,
) Instruction {
    return Instruction{
        .mnemonic = mnemonic,
        .byte_length = byte_length,
        .clock_cycles = clock_cycles,
    };
}

//
// Glossary of syntax
//
//      A, B,
//      C, D,
//      E, H,
//      L, F    - 8-bit registers of the gameboy
//      Z, N,
//      H, C    - register flags in upper bits of F
//      %X      - unsigned 8 bit value read from memory
//      %Xi     - SIGNED 8 bit value read from memoru
//      %XX     - unsigned 16 bit value read from memory
//      $00ABCD - constant hexadecimal value
//      (V)     - read/write from memory at address given by V
//      V+/V-   - increment/decrement V after operation
//

pub fn instructions_details(ix: u8) ?Instruction {
    return switch (ix) {
        // 0x00 Prefixed instructions
        0x00 => instruction("nop", 1, 4),
        0x01 => instruction("ld (BC) %XX", 3, 12),
        0x02 => instruction("ld (BC) A", 1, 8),
        0x03 => instruction("inc BC", 1, 8),
        0x04 => instruction("inc B", 1, 4),
        0x05 => instruction("dec B", 1, 4),
        0x06 => instruction("ld B %X", 2, 8),
        0x07 => instruction("rcla", 1, 4),
        0x08 => instruction("ld (%XX) SP", 3, 20),
        0x09 => instruction("add HL BC", 1, 8),
        0x0A => instruction("ld A (BC)", 1, 8),
        0x0B => instruction("dec BC", 1, 8),
        0x0C => instruction("inc C", 1, 4),
        0x0D => instruction("dec C", 1, 4),
        0x0E => instruction("ld C %X", 2, 8),
        0x0F => instruction("rrca", 1, 4),

        // 0x10 Prefixed instructions
        0x10 => instruction("stop", 1, 4),
        0x11 => instruction("ld DE %XX", 3, 12),
        0x12 => instruction("ld (DE) A", 1, 8),
        0x13 => instruction("inc DE", 1, 8),
        0x14 => instruction("inc D", 1, 4),
        0x15 => instruction("dec D", 1, 4),
        0x16 => instruction("ld D %X", 2, 8),
        0x17 => instruction("rla", 1, 4),
        0x18 => instruction("jr %Xi", 2, 10),
        0x19 => instruction("add HL DE", 1, 8),
        0x1A => instruction("ld A (DE)", 1, 8),
        0x1B => instruction("dec DE", 1, 8),
        0x1C => instruction("inc E", 1, 4),
        0x1D => instruction("dec E", 1, 4),
        0x1E => instruction("ld E %X", 2, 8),
        0x1F => instruction("rra", 1, 4),

        // 0x20 Prefixed instructions
        0x20 => instruction("jr NZ %Xi", 2, (8 + 12) / 2),
        0x21 => instruction("ld HL %XX", 3, 12),
        0x22 => instruction("ldi (HL) A", 1, 8),
        0x23 => instruction("inc HL", 1, 8),
        0x24 => instruction("inc H", 1, 4),
        0x25 => instruction("dec H", 1, 4),
        0x26 => instruction("ld H %X", 2, 8),
        0x27 => instruction("daa", 1, 4),
        0x28 => instruction("jr Z %Xi", 2, (8 + 12) / 2),
        0x29 => instruction("add HL HL", 1, 8),
        0x2A => instruction("ldi A (HL)", 1, 8),
        0x2B => instruction("dec HL", 1, 8),
        0x2C => instruction("inc L", 1, 4),
        0x2D => instruction("dec L", 1, 4),
        0x2E => instruction("ld L %X", 2, 8),
        0x2F => instruction("cpl", 1, 4),

        // 0x30 Prefixed instructions
        0x30 => instruction("jr NC %Xi", 2, (8 + 12) / 2),
        0x31 => instruction("ld SP %XX", 3, 12),
        0x32 => instruction("ldd (HL) A", 1, 8),
        0x33 => instruction("inc sp", 1, 8),
        0x34 => instruction("inc (HL)", 1, 12),
        0x35 => instruction("dec (HL)", 1, 12),
        0x36 => instruction("ld (HL) %X", 2, 12),
        0x37 => instruction("scf", 1, 4),
        0x38 => instruction("jr C %Xi", 2, (8 + 12) / 2),
        0x39 => instruction("add HL SP", 1, 8),
        0x3A => instruction("ldd A (HL)", 1, 8),
        0x3B => instruction("dec SP", 1, 8),
        0x3C => instruction("inc A", 1, 4),
        0x3D => instruction("dec A", 1, 4),
        0x3E => instruction("ld A %X", 2, 8),
        0x3F => instruction("ccf", 1, 4),

        // 0x40 Prefixed instructions
        0x40 => instruction("ld B B", 1, 4),
        0x41 => instruction("ld B C", 1, 4),
        0x42 => instruction("ld B D", 1, 4),
        0x43 => instruction("ld B E", 1, 4),
        0x44 => instruction("ld B H", 1, 4),
        0x45 => instruction("ld B L", 1, 4),
        0x46 => instruction("ld B (HL)", 1, 8),
        0x47 => instruction("ld B A", 1, 4),
        0x48 => instruction("ld C B", 1, 4),
        0x49 => instruction("ld C C", 1, 4),
        0x4A => instruction("ld C D", 1, 4),
        0x4B => instruction("ld C E", 1, 4),
        0x4C => instruction("ld C H", 1, 4),
        0x4D => instruction("ld C L", 1, 4),
        0x4E => instruction("ld C (HL)", 1, 8),
        0x4F => instruction("ld C A", 1, 4),

        // 0x50 Prefixed instructions
        0x50 => instruction("ld D B", 1, 4),
        0x51 => instruction("ld D C", 1, 4),
        0x52 => instruction("ld D D", 1, 4),
        0x53 => instruction("ld D E", 1, 4),
        0x54 => instruction("ld D H", 1, 4),
        0x55 => instruction("ld D L", 1, 4),
        0x56 => instruction("ld D (HL)", 1, 8),
        0x57 => instruction("ld D A", 1, 4),
        0x58 => instruction("ld E B", 1, 4),
        0x59 => instruction("ld E C", 1, 4),
        0x5A => instruction("ld E D", 1, 4),
        0x5B => instruction("ld E E", 1, 4),
        0x5C => instruction("ld E H", 1, 4),
        0x5D => instruction("ld E L", 1, 4),
        0x5E => instruction("ld E (HL)", 1, 8),
        0x5F => instruction("ld E A", 1, 4),

        // 0x60 Prefixed instructions
        0x60 => instruction("ld H B", 1, 4),
        0x61 => instruction("ld H C", 1, 4),
        0x62 => instruction("ld H D", 1, 4),
        0x63 => instruction("ld H E", 1, 4),
        0x64 => instruction("ld H H", 1, 4),
        0x65 => instruction("ld H L", 1, 4),
        0x66 => instruction("ld H (HL)", 1, 8),
        0x67 => instruction("ld H A", 1, 4),
        0x68 => instruction("ld L B", 1, 4),
        0x69 => instruction("ld L C", 1, 4),
        0x6A => instruction("ld L D", 1, 4),
        0x6B => instruction("ld L E", 1, 4),
        0x6C => instruction("ld L H", 1, 4),
        0x6D => instruction("ld L L", 1, 4),
        0x6E => instruction("ld L (HL)", 1, 8),
        0x6F => instruction("ld L A", 1, 4),

        // 0x70 Prefixed instructions
        0x70 => instruction("ld (HL) B", 1, 4),
        0x71 => instruction("ld (HL) C", 1, 4),
        0x72 => instruction("ld (HL) D", 1, 4),
        0x73 => instruction("ld (HL) E", 1, 8),
        0x74 => instruction("ld (HL) H", 1, 4),
        0x75 => instruction("ld (HL) L", 1, 4),
        0x76 => instruction("halt", 1, 4),
        0x77 => instruction("ld (HL) A", 1, 8),
        0x78 => instruction("ld A B", 1, 4),
        0x79 => instruction("ld A C", 1, 4),
        0x7A => instruction("ld A D", 1, 4),
        0x7B => instruction("ld A E", 1, 4),
        0x7C => instruction("ld A H", 1, 4),
        0x7D => instruction("ld A L", 1, 4),
        0x7E => instruction("ld A (HL)", 1, 8),
        0x7F => instruction("ld A A", 1, 4),

        // 0x80 Prefixed instructions
        0x80 => null,
        0x81 => null,
        0x82 => null,
        0x83 => null,
        0x84 => null,
        0x85 => null,
        0x86 => null,
        0x87 => null,
        0x88 => null,
        0x89 => null,
        0x8A => null,
        0x8B => null,
        0x8C => null,
        0x8D => null,
        0x8E => null,
        0x8F => null,

        // 0x90 Prefixed instructions
        0x90 => instruction("sub A B", 1, 4),
        0x91 => null,
        0x92 => null,
        0x93 => null,
        0x94 => null,
        0x95 => null,
        0x96 => null,
        0x97 => null,
        0x98 => null,
        0x99 => null,
        0x9A => null,
        0x9B => null,
        0x9C => null,
        0x9D => null,
        0x9E => null,
        0x9F => null,

        // 0xA0 Prefixed instructions
        0xA0 => null,
        0xA1 => null,
        0xA2 => null,
        0xA3 => null,
        0xA4 => null,
        0xA5 => null,
        0xA6 => null,
        0xA7 => null,
        0xA8 => null,
        0xA9 => null,
        0xAA => null,
        0xAB => null,
        0xAC => null,
        0xAD => null,
        0xAE => null,
        0xAF => instruction("xor A A", 1, 4),

        // 0xB0 Prefixed instructions
        0xB0 => null,
        0xB1 => null,
        0xB2 => null,
        0xB3 => null,
        0xB4 => null,
        0xB5 => null,
        0xB6 => null,
        0xB7 => null,
        0xB8 => null,
        0xB9 => null,
        0xBA => null,
        0xBB => null,
        0xBC => null,
        0xBD => null,
        0xBE => null,
        0xBF => null,

        // 0xC0 Prefixed instructions
        0xC0 => instruction("ret NZ", 1, 14),
        0xC1 => instruction("pop BC", 1, 12),
        0xC2 => instruction("jp NZ %XX", 3, 14),
        0xC3 => instruction("jp %XX", 3, 16),
        0xC4 => instruction("call NZ %XX", 3, 16),
        0xC5 => instruction("push BC", 1, 16),
        0xC6 => instruction("add A %X", 2, 8),
        0xC7 => instruction("rst 00h", 1, 16),
        0xC8 => instruction("ret Z", 1, 14),
        0xC9 => instruction("ret", 1, 16),
        0xCA => instruction("jp Z %XX", 3, 14),
        0xCB => unreachable, // Prefix
        0xCC => instruction("call Z %XX", 3, 18),
        0xCD => instruction("call %XX", 3, 24),
        0xCE => instruction("adc A %X", 2, 8),
        0xCF => instruction("rst 08h", 1, 16),

        // 0xD0 Prefixed instructions
        0xD0 => null,
        0xD1 => null,
        0xD2 => null,
        0xD3 => null,
        0xD4 => null,
        0xD5 => null,
        0xD6 => null,
        0xD7 => null,
        0xD8 => null,
        0xD9 => null,
        0xDA => null,
        0xDB => null,
        0xDC => null,
        0xDD => null,
        0xDE => null,
        0xDF => null,

        // 0xE0 Prefixed instructions
        0xE0 => instruction("ld ($FF00 + %X) A", 2, 12),
        0xE1 => null,
        0xE2 => instruction("ld ($FF00 + C) A", 1, 8),
        0xE3 => null,
        0xE4 => null,
        0xE5 => null,
        0xE6 => null,
        0xE7 => null,
        0xE8 => null,
        0xE9 => null,
        0xEA => instruction("ld (%XX) A", 3, 16),
        0xEB => null,
        0xEC => null,
        0xED => null,
        0xEE => null,
        0xEF => null,

        // 0xF0 Prefixed instructions
        0xF0 => instruction("ld A ($FF00 + %X)", 2, 12),
        0xF1 => null,
        0xF2 => null,
        0xF3 => instruction("di", 1, 4),
        0xF4 => null,
        0xF5 => null,
        0xF6 => null,
        0xF7 => null,
        0xF8 => null,
        0xF9 => null,
        0xFA => null,
        0xFB => instruction("ei", 1, 4),
        0xFC => null,
        0xFD => null,
        0xFE => instruction("cp A %X", 2, 8),
        0xFF => null,
    };
}

pub fn prefixed_instructions_details(ix: u8) ?Instruction {
    return switch (ix) {
        0x11 => instruction("rl C", 1, 8),

        0x7C => instruction("bit 7 H", 1, 8),

        else => null,
    };
}
