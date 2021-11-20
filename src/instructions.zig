const std = @import("std");

/// Speed of GameBoy CPU in number of clock cycles
/// (also called t-cycles) per second.
///
/// `1 Mhz = 10 ** 6 Clocks per second`
pub const Speed = comptime try std.math.powi(usize, 10, 6);

pub const FlagRegister = packed struct {
    unused: u4 = 0, // lower nibble of the flag register
    carry: bool = false, // set if a carry occured or A is smaller in the CP instruction
    half: bool = false, // set if a carry occured in the lower nibble of the last operation
    nsub: bool = false, // set if subtraction occured in the last operation
    zero: bool = false, // set if last operation resulted in zero, or CMP'd values matched
};

pub const Registers = struct {
    sp: u16 = 0, // 16-bit stack pointer

    pc: u16 = 0, // 16-bit program counter

    ime: bool = true, // interrupts enabled

    af: packed union {
        all: u16,
        ind: packed struct {
            f: FlagRegister,
            a: u8,
        },
    } = .{ .all = 0 },

    bc: packed union {
        all: u16,
        ind: packed struct {
            c: u8,
            b: u8,
        },
    } = .{ .all = 0 },

    de: packed union {
        all: u16,
        ind: packed struct {
            e: u8,
            d: u8,
        },
    } = .{ .all = 0 },

    hl: packed union {
        all: u16,
        ind: packed struct {
            l: u8,
            h: u8,
        },
    } = .{ .all = 0 },
};

pub const Controller = packed struct {
    right_or_a_button: bool,
    left_or_b_button: bool,
    down_or_start: bool,
    up_or_select: bool,
    select_direction: bool,
    select_action: bool,
    unused: u2,
};

pub const TimerControl = packed struct {
    mode: enum(u2) {
        slowest = 0b00,
        fastest = 0b01,
        fast = 0b10,
        slow = 0b11,
    },

    enabled: bool,
    unused: u5,
};

pub const Interrupt = enum {
    v_blank,
    lcd_status,
    timer,
    serial,
    joypad,
};

pub const InterruptFlags = packed struct {
    v_blank: bool,
    lcd_status: bool,
    timer: bool,
    serial: bool,
    joypad: bool,
    unused: u4,

    pub fn is_set(self: *@This(), comptime irrpt: Interrupt) bool {
        return @field(self, comptime switch (irrpt) {
            .v_blank => "v_blank",
            .lcd_status => "lcd_status",
            .timer => "timer",
            .serial => "serial",
            .joypad => "joypad",
        });
    }

    pub fn set(self: *@This(), comptime irrpt: Interrupt, value: bool) void {
        @field(self, comptime switch (irrpt) {
            .v_blank => "v_blank",
            .lcd_status => "lcd_status",
            .timer => "timer",
            .serial => "serial",
            .joypad => "joypad",
        }) = value;
    }
};

pub const LCDControl = packed struct {
    bg_win_enable: bool,
    sprites_enable: bool,
    sprites_tall: bool,
    background_tilemap: u1,
    bg_win_tiledata: u1,
    window_enable: bool,
    window_tilemap: u1,
    lcd_ppu_enable: bool,
};

pub const LCDStatus = packed struct {
    draw_stage: enum(u2) {
        h_blank = 0,
        v_blank = 1,
        oam_scan = 2,
        pixel_transfer = 3,
    },

    lyc_equal_ly: bool,
    hblank_interrupt: bool,
    vblank_interrupt: bool,
    oam_interrupt: bool,
    lyc_interrupt: bool,
    unused: u1,
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

fn control_flow(mnemonic: []const u8, clock_cycles: usize) Instruction {
    // Control flow instructions move the PC manually, so
    // we shouldn't move it based on instruction byte length.
    return instruction(mnemonic, 0, clock_cycles);
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
        // 0x00 instructions
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

        // 0x10 instructions
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

        // 0x20 instructions
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

        // 0x30 instructions
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

        // 0x40 instructions
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

        // 0x50 instructions
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

        // 0x60 instructions
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

        // 0x70 instructions
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

        // 0x80 instructions
        0x80 => instruction("add A B", 1, 4),
        0x81 => instruction("add A C", 1, 4),
        0x82 => instruction("add A D", 1, 4),
        0x83 => instruction("add A E", 1, 4),
        0x84 => instruction("add A H", 1, 4),
        0x85 => instruction("add A L", 1, 4),
        0x86 => instruction("add A (HL)", 1, 8),
        0x87 => instruction("add A A", 1, 4),
        0x88 => instruction("adc A B", 1, 4),
        0x89 => instruction("adc A C", 1, 4),
        0x8A => instruction("adc A D", 1, 4),
        0x8B => instruction("adc A E", 1, 4),
        0x8C => instruction("adc A H", 1, 4),
        0x8D => instruction("adc A L", 1, 4),
        0x8E => instruction("adc A (HL)", 1, 8),
        0x8F => instruction("adc A A", 1, 4),

        // 0x90 instructions
        0x90 => instruction("sub A B", 1, 4),
        0x91 => instruction("sub A C", 1, 4),
        0x92 => instruction("sub A D", 1, 4),
        0x93 => instruction("sub A E", 1, 4),
        0x94 => instruction("sub A H", 1, 4),
        0x95 => instruction("sub A L", 1, 4),
        0x96 => instruction("sub A (HL)", 1, 8),
        0x97 => instruction("sub A A", 1, 4),
        0x98 => instruction("sbc A B", 1, 4),
        0x99 => instruction("sbc A C", 1, 4),
        0x9A => instruction("sbc A D", 1, 4),
        0x9B => instruction("sbc A E", 1, 4),
        0x9C => instruction("sbc A H", 1, 4),
        0x9D => instruction("sbc A L", 1, 4),
        0x9E => instruction("sbc A (HL)", 1, 8),
        0x9F => instruction("sbc A A", 1, 4),

        // 0xA0 instructions
        0xA0 => instruction("and A B", 1, 4),
        0xA1 => instruction("and A C", 1, 4),
        0xA2 => instruction("and A D", 1, 4),
        0xA3 => instruction("and A E", 1, 4),
        0xA4 => instruction("and A H", 1, 4),
        0xA5 => instruction("and A L", 1, 4),
        0xA6 => instruction("and A (HL)", 1, 8),
        0xA7 => instruction("and A A", 1, 4),
        0xA8 => instruction("xor A B", 1, 4),
        0xA9 => instruction("xor A C", 1, 4),
        0xAA => instruction("xor A D", 1, 4),
        0xAB => instruction("xor A E", 1, 4),
        0xAC => instruction("xor A H", 1, 4),
        0xAD => instruction("xor A L", 1, 4),
        0xAE => instruction("xor A (HL)", 1, 8),
        0xAF => instruction("xor A A", 1, 4),

        // 0xB0 instructions
        0xB0 => instruction("or A B", 1, 4),
        0xB1 => instruction("or A C", 1, 4),
        0xB2 => instruction("or A D", 1, 4),
        0xB3 => instruction("or A E", 1, 4),
        0xB4 => instruction("or A H", 1, 4),
        0xB5 => instruction("or A L", 1, 4),
        0xB6 => instruction("or A (HL)", 1, 8),
        0xB7 => instruction("or A A", 1, 4),
        0xB8 => instruction("cp A B", 1, 4),
        0xB9 => instruction("cp A C", 1, 4),
        0xBA => instruction("cp A D", 1, 4),
        0xBB => instruction("cp A E", 1, 4),
        0xBC => instruction("cp A H", 1, 4),
        0xBD => instruction("cp A L", 1, 4),
        0xBE => instruction("cp A (HL)", 1, 8),
        0xBF => instruction("cp A A", 1, 4),

        // 0xC0 instructions
        0xC0 => control_flow("ret NZ", 14),
        0xC1 => instruction("pop BC", 1, 12),
        0xC2 => control_flow("jp NZ %XX", 14),
        0xC3 => control_flow("jp %XX", 16),
        0xC4 => control_flow("call NZ %XX", 16),
        0xC5 => instruction("push BC", 1, 16),
        0xC6 => instruction("add A %X", 2, 8),
        0xC7 => control_flow("rst 00h", 16),
        0xC8 => control_flow("ret Z", 14),
        0xC9 => control_flow("ret", 16),
        0xCA => control_flow("jp Z %XX", 14),
        0xCB => unreachable, // Prefix
        0xCC => control_flow("call Z %XX", 18),
        0xCD => control_flow("call %XX", 24),
        0xCE => instruction("adc A %X", 2, 8),
        0xCF => control_flow("rst 08h", 16),

        // 0xD0 instructions
        0xD0 => control_flow("ret NC", (8 + 20) / 2),
        0xD1 => instruction("pop DE", 1, 12),
        0xD2 => control_flow("jp NC %XX", (12 + 16) / 2),
        0xD3 => null, // Non-existant
        0xD4 => control_flow("call NC %XX", (12 + 24) / 2),
        0xD5 => instruction("push DE", 1, 16),
        0xD6 => instruction("sub A %X", 2, 8),
        0xD7 => control_flow("rst 10h", 16),
        0xD8 => control_flow("ret C", (8 + 16) / 2),
        0xD9 => control_flow("reti", 16),
        0xDA => control_flow("jp C %XX", (12 + 16) / 2),
        0xDB => null, // Non-existant
        0xDC => control_flow("call C %XX", (12 + 24) / 2),
        0xDD => null, // Non-existant
        0xDE => instruction("sbc A %X", 2, 8),
        0xDF => control_flow("rst 18h", 16),

        // 0xE0 instructions
        0xE0 => instruction("ld ($FF00 + %X) A", 2, 12),
        0xE1 => instruction("pop HL", 1, 12),
        0xE2 => instruction("ld ($FF00 + C) A", 1, 8),
        0xE3 => null, // Non-existant
        0xE4 => null, // Non-existant
        0xE5 => instruction("push HL", 1, 16),
        0xE6 => instruction("and A %X", 2, 8),
        0xE7 => control_flow("rst 20h", 16),
        0xE8 => instruction("add SP %Xi", 2, 16),
        0xE9 => control_flow("jp HL", 4),
        0xEA => instruction("ld (%XX) A", 3, 16),
        0xEB => null, // Non-existant
        0xEC => null, // Non-existant
        0xED => null, // Non-existant
        0xEE => instruction("xor A %X", 2, 8),
        0xEF => control_flow("rst 28h", 16),

        // 0xF0 instructions
        0xF0 => instruction("ld A ($FF00 + %X)", 2, 12),
        0xF1 => instruction("pop AF", 1, 12),
        0xF2 => instruction("ld A ($FF00 + C)", 1, 8),
        0xF3 => instruction("di", 1, 4),
        0xF4 => null, // Non-existant
        0xF5 => instruction("push AF", 1, 16),
        0xF6 => instruction("or A %X", 2, 8),
        0xF7 => control_flow("rst 30h", 16),
        0xF8 => instruction("ld HL SP+%Xi", 2, 12),
        0xF9 => instruction("ld SP HL", 1, 8),
        0xFA => instruction("ld A (%XX)", 3, 16),
        0xFB => instruction("ei", 1, 4),
        0xFC => null, // Non-existant
        0xFD => null, // Non-existant
        0xFE => instruction("cp A %X", 2, 8),
        0xFF => control_flow("rst 38h", 16),
    };
}

pub fn prefixed_instructions_details(ix: u8) ?Instruction {
    return switch (ix) {
        0x00 => instruction("rlc B", 1, 8),
        0x01 => instruction("rlc C", 1, 8),
        0x02 => instruction("rlc D", 1, 8),
        0x03 => instruction("rlc E", 1, 8),
        0x04 => instruction("rlc H", 1, 8),
        0x05 => instruction("rlc L", 1, 8),
        0x06 => instruction("rlc (HL)", 1, 16),
        0x07 => instruction("rlc A", 1, 8),
        0x08 => instruction("rrc B", 1, 8),
        0x09 => instruction("rrc C", 1, 8),
        0x0A => instruction("rrc D", 1, 8),
        0x0B => instruction("rrc E", 1, 8),
        0x0C => instruction("rrc H", 1, 8),
        0x0D => instruction("rrc L", 1, 8),
        0x0E => instruction("rrc (HL)", 1, 16),
        0x0F => instruction("rrc A", 1, 8),

        0x10 => instruction("rl B", 1, 8),
        0x11 => instruction("rl C", 1, 8),
        0x12 => instruction("rl D", 1, 8),
        0x13 => instruction("rl E", 1, 8),
        0x14 => instruction("rl H", 1, 8),
        0x15 => instruction("rl L", 1, 8),
        0x16 => instruction("rl (HL)", 1, 16),
        0x17 => instruction("rl A", 1, 8),
        0x18 => instruction("rr B", 1, 8),
        0x19 => instruction("rr C", 1, 8),
        0x1A => instruction("rr D", 1, 8),
        0x1B => instruction("rr E", 1, 8),
        0x1C => instruction("rr H", 1, 8),
        0x1D => instruction("rr L", 1, 8),
        0x1E => instruction("rr (HL)", 1, 16),
        0x1F => instruction("rr A", 1, 8),

        0x20 => instruction("sla B", 1, 8),
        0x21 => instruction("sla C", 1, 8),
        0x22 => instruction("sla D", 1, 8),
        0x23 => instruction("sla E", 1, 8),
        0x24 => instruction("sla H", 1, 8),
        0x25 => instruction("sla L", 1, 8),
        0x26 => instruction("sla (HL)", 1, 16),
        0x27 => instruction("sla A", 1, 8),
        0x28 => instruction("sra B", 1, 8),
        0x29 => instruction("sra C", 1, 8),
        0x2A => instruction("sra D", 1, 8),
        0x2B => instruction("sra E", 1, 8),
        0x2C => instruction("sra H", 1, 8),
        0x2D => instruction("sra L", 1, 8),
        0x2E => instruction("sra (HL)", 1, 16),
        0x2F => instruction("sra A", 1, 8),

        0x30 => instruction("swap B", 1, 8),
        0x31 => instruction("swap C", 1, 8),
        0x32 => instruction("swap D", 1, 8),
        0x33 => instruction("swap E", 1, 8),
        0x34 => instruction("swap H", 1, 8),
        0x35 => instruction("swap L", 1, 8),
        0x36 => instruction("swap (HL)", 1, 16),
        0x37 => instruction("swap A", 1, 8),
        0x38 => instruction("srl B", 1, 8),
        0x39 => instruction("srl C", 1, 8),
        0x3A => instruction("srl D", 1, 8),
        0x3B => instruction("srl E", 1, 8),
        0x3C => instruction("srl H", 1, 8),
        0x3D => instruction("srl L", 1, 8),
        0x3E => instruction("srl (HL)", 1, 16),
        0x3F => instruction("srl A", 1, 8),

        0x40 => instruction("bit 0 B", 1, 8),
        0x41 => instruction("bit 0 C", 1, 8),
        0x42 => instruction("bit 0 D", 1, 8),
        0x43 => instruction("bit 0 E", 1, 8),
        0x44 => instruction("bit 0 H", 1, 8),
        0x45 => instruction("bit 0 L", 1, 8),
        0x46 => instruction("bit 0 (HL)", 1, 16),
        0x47 => instruction("bit 0 A", 1, 8),
        0x48 => instruction("bit 1 B", 1, 8),
        0x49 => instruction("bit 1 C", 1, 8),
        0x4A => instruction("bit 1 D", 1, 8),
        0x4B => instruction("bit 1 E", 1, 8),
        0x4C => instruction("bit 1 H", 1, 8),
        0x4D => instruction("bit 1 L", 1, 8),
        0x4E => instruction("bit 1 (HL)", 1, 16),
        0x4F => instruction("bit 1 A", 1, 8),

        0x50 => instruction("bit 2 B", 1, 8),
        0x51 => instruction("bit 2 C", 1, 8),
        0x52 => instruction("bit 2 D", 1, 8),
        0x53 => instruction("bit 2 E", 1, 8),
        0x54 => instruction("bit 2 H", 1, 8),
        0x55 => instruction("bit 2 L", 1, 8),
        0x56 => instruction("bit 2 (HL)", 1, 16),
        0x57 => instruction("bit 2 A", 1, 8),
        0x58 => instruction("bit 3 B", 1, 8),
        0x59 => instruction("bit 3 C", 1, 8),
        0x5A => instruction("bit 3 D", 1, 8),
        0x5B => instruction("bit 3 E", 1, 8),
        0x5C => instruction("bit 3 H", 1, 8),
        0x5D => instruction("bit 3 L", 1, 8),
        0x5E => instruction("bit 3 (HL)", 1, 16),
        0x5F => instruction("bit 3 A", 1, 8),

        0x60 => instruction("bit 4 B", 1, 8),
        0x61 => instruction("bit 4 C", 1, 8),
        0x62 => instruction("bit 4 D", 1, 8),
        0x63 => instruction("bit 4 E", 1, 8),
        0x64 => instruction("bit 4 H", 1, 8),
        0x65 => instruction("bit 4 L", 1, 8),
        0x66 => instruction("bit 4 (HL)", 1, 16),
        0x67 => instruction("bit 4 A", 1, 8),
        0x68 => instruction("bit 5 B", 1, 8),
        0x69 => instruction("bit 5 C", 1, 8),
        0x6A => instruction("bit 5 D", 1, 8),
        0x6B => instruction("bit 5 E", 1, 8),
        0x6C => instruction("bit 5 H", 1, 8),
        0x6D => instruction("bit 5 L", 1, 8),
        0x6E => instruction("bit 5 (HL)", 1, 16),
        0x6F => instruction("bit 5 A", 1, 8),

        0x70 => instruction("bit 6 B", 1, 8),
        0x71 => instruction("bit 6 C", 1, 8),
        0x72 => instruction("bit 6 D", 1, 8),
        0x73 => instruction("bit 6 E", 1, 8),
        0x74 => instruction("bit 6 H", 1, 8),
        0x75 => instruction("bit 6 L", 1, 8),
        0x76 => instruction("bit 6 (HL)", 1, 16),
        0x77 => instruction("bit 6 A", 1, 8),
        0x78 => instruction("bit 7 B", 1, 8),
        0x79 => instruction("bit 7 C", 1, 8),
        0x7A => instruction("bit 7 D", 1, 8),
        0x7B => instruction("bit 7 E", 1, 8),
        0x7C => instruction("bit 7 H", 1, 8),
        0x7D => instruction("bit 7 L", 1, 8),
        0x7E => instruction("bit 7 (HL)", 1, 16),
        0x7F => instruction("bit 7 A", 1, 8),

        0x80 => instruction("res 0 B", 1, 8),
        0x81 => instruction("res 0 C", 1, 8),
        0x82 => instruction("res 0 D", 1, 8),
        0x83 => instruction("res 0 E", 1, 8),
        0x84 => instruction("res 0 H", 1, 8),
        0x85 => instruction("res 0 L", 1, 8),
        0x86 => instruction("res 0 (HL)", 1, 16),
        0x87 => instruction("res 0 A", 1, 8),
        0x88 => instruction("res 1 B", 1, 8),
        0x89 => instruction("res 1 C", 1, 8),
        0x8A => instruction("res 1 D", 1, 8),
        0x8B => instruction("res 1 E", 1, 8),
        0x8C => instruction("res 1 H", 1, 8),
        0x8D => instruction("res 1 L", 1, 8),
        0x8E => instruction("res 1 (HL)", 1, 16),
        0x8F => instruction("res 1 A", 1, 8),

        0x90 => instruction("res 2 B", 1, 8),
        0x91 => instruction("res 2 C", 1, 8),
        0x92 => instruction("res 2 D", 1, 8),
        0x93 => instruction("res 2 E", 1, 8),
        0x94 => instruction("res 2 H", 1, 8),
        0x95 => instruction("res 2 L", 1, 8),
        0x96 => instruction("res 2 (HL)", 1, 16),
        0x97 => instruction("res 2 A", 1, 8),
        0x98 => instruction("res 3 B", 1, 8),
        0x99 => instruction("res 3 C", 1, 8),
        0x9A => instruction("res 3 D", 1, 8),
        0x9B => instruction("res 3 E", 1, 8),
        0x9C => instruction("res 3 H", 1, 8),
        0x9D => instruction("res 3 L", 1, 8),
        0x9E => instruction("res 3 (HL)", 1, 16),
        0x9F => instruction("res 3 A", 1, 8),

        0xA0 => instruction("res 4 B", 1, 8),
        0xA1 => instruction("res 4 C", 1, 8),
        0xA2 => instruction("res 4 D", 1, 8),
        0xA3 => instruction("res 4 E", 1, 8),
        0xA4 => instruction("res 4 H", 1, 8),
        0xA5 => instruction("res 4 L", 1, 8),
        0xA6 => instruction("res 4 (HL)", 1, 16),
        0xA7 => instruction("res 4 A", 1, 8),
        0xA8 => instruction("res 5 B", 1, 8),
        0xA9 => instruction("res 5 C", 1, 8),
        0xAA => instruction("res 5 D", 1, 8),
        0xAB => instruction("res 5 E", 1, 8),
        0xAC => instruction("res 5 H", 1, 8),
        0xAD => instruction("res 5 L", 1, 8),
        0xAE => instruction("res 5 (HL)", 1, 16),
        0xAF => instruction("res 5 A", 1, 8),

        0xB0 => instruction("res 6 B", 1, 8),
        0xB1 => instruction("res 6 C", 1, 8),
        0xB2 => instruction("res 6 D", 1, 8),
        0xB3 => instruction("res 6 E", 1, 8),
        0xB4 => instruction("res 6 H", 1, 8),
        0xB5 => instruction("res 6 L", 1, 8),
        0xB6 => instruction("res 6 (HL)", 1, 16),
        0xB7 => instruction("res 6 A", 1, 8),
        0xB8 => instruction("res 7 B", 1, 8),
        0xB9 => instruction("res 7 C", 1, 8),
        0xBA => instruction("res 7 D", 1, 8),
        0xBB => instruction("res 7 E", 1, 8),
        0xBC => instruction("res 7 H", 1, 8),
        0xBD => instruction("res 7 L", 1, 8),
        0xBE => instruction("res 7 (HL)", 1, 16),
        0xBF => instruction("res 7 A", 1, 8),

        0xC0 => instruction("set 0 B", 1, 8),
        0xC1 => instruction("set 0 C", 1, 8),
        0xC2 => instruction("set 0 D", 1, 8),
        0xC3 => instruction("set 0 E", 1, 8),
        0xC4 => instruction("set 0 H", 1, 8),
        0xC5 => instruction("set 0 L", 1, 8),
        0xC6 => instruction("set 0 (HL)", 1, 16),
        0xC7 => instruction("set 0 A", 1, 8),
        0xC8 => instruction("set 1 B", 1, 8),
        0xC9 => instruction("set 1 C", 1, 8),
        0xCA => instruction("set 1 D", 1, 8),
        0xCB => instruction("set 1 E", 1, 8),
        0xCC => instruction("set 1 H", 1, 8),
        0xCD => instruction("set 1 L", 1, 8),
        0xCE => instruction("set 1 (HL)", 1, 16),
        0xCF => instruction("set 1 A", 1, 8),

        0xD0 => instruction("set 2 B", 1, 8),
        0xD1 => instruction("set 2 C", 1, 8),
        0xD2 => instruction("set 2 D", 1, 8),
        0xD3 => instruction("set 2 E", 1, 8),
        0xD4 => instruction("set 2 H", 1, 8),
        0xD5 => instruction("set 2 L", 1, 8),
        0xD6 => instruction("set 2 (HL)", 1, 16),
        0xD7 => instruction("set 2 A", 1, 8),
        0xD8 => instruction("set 3 B", 1, 8),
        0xD9 => instruction("set 3 C", 1, 8),
        0xDA => instruction("set 3 D", 1, 8),
        0xDB => instruction("set 3 E", 1, 8),
        0xDC => instruction("set 3 H", 1, 8),
        0xDD => instruction("set 3 L", 1, 8),
        0xDE => instruction("set 3 (HL)", 1, 16),
        0xDF => instruction("set 3 A", 1, 8),

        0xE0 => instruction("set 4 B", 1, 8),
        0xE1 => instruction("set 4 C", 1, 8),
        0xE2 => instruction("set 4 D", 1, 8),
        0xE3 => instruction("set 4 E", 1, 8),
        0xE4 => instruction("set 4 H", 1, 8),
        0xE5 => instruction("set 4 L", 1, 8),
        0xE6 => instruction("set 4 (HL)", 1, 16),
        0xE7 => instruction("set 4 A", 1, 8),
        0xE8 => instruction("set 5 B", 1, 8),
        0xE9 => instruction("set 5 C", 1, 8),
        0xEA => instruction("set 5 D", 1, 8),
        0xEB => instruction("set 5 E", 1, 8),
        0xEC => instruction("set 5 H", 1, 8),
        0xED => instruction("set 5 L", 1, 8),
        0xEE => instruction("set 5 (HL)", 1, 16),
        0xEF => instruction("set 5 A", 1, 8),

        0xF0 => instruction("set 6 B", 1, 8),
        0xF1 => instruction("set 6 C", 1, 8),
        0xF2 => instruction("set 6 D", 1, 8),
        0xF3 => instruction("set 6 E", 1, 8),
        0xF4 => instruction("set 6 H", 1, 8),
        0xF5 => instruction("set 6 L", 1, 8),
        0xF6 => instruction("set 6 (HL)", 1, 16),
        0xF7 => instruction("set 6 A", 1, 8),
        0xF8 => instruction("set 7 B", 1, 8),
        0xF9 => instruction("set 7 C", 1, 8),
        0xFA => instruction("set 7 D", 1, 8),
        0xFB => instruction("set 7 E", 1, 8),
        0xFC => instruction("set 7 H", 1, 8),
        0xFD => instruction("set 7 L", 1, 8),
        0xFE => instruction("set 7 (HL)", 1, 16),
        0xFF => instruction("set 7 A", 1, 8),
    };
}
