const std = @import("std");

const MMU = @import("memory.zig");
const PPU = @import("graphics.zig");
const CPU = @import("execution.zig");

const instr = @import("instructions.zig");

const Window = @import("window.zig").Window;

const Self = @This();

fn HashSet(comptime K: type) type {
    return std.AutoHashMap(K, void);
}

mmu: *MMU,
ppu: *PPU,
cpu: *CPU,
stdin: std.fs.File.Reader,
stdout: std.fs.File.Writer,
allocator: std.mem.Allocator,

quit: bool,
broke: bool,
window: ?Window,
breakpoints: HashSet(u16),
print_states: struct {
    cpu_state: bool,
    ppu_state: bool,
    tiledata: HashSet(u16),
    tilemap: HashSet(u1),
},

pub fn init(
    allocator: std.mem.Allocator,
    mmu: *MMU,
    ppu: *PPU,
    cpu: *CPU,
) Self {
    return Self{
        .mmu = mmu,
        .ppu = ppu,
        .cpu = cpu,
        .stdin = std.io.getStdIn().reader(),
        .stdout = std.io.getStdOut().writer(),
        .allocator = allocator,

        .quit = false,
        .broke = false,
        .window = null,
        .breakpoints = HashSet(u16).init(allocator),
        .print_states = .{
            .cpu_state = false,
            .ppu_state = false,
            .tiledata = HashSet(u16).init(allocator),
            .tilemap = HashSet(u1).init(allocator),
        },
    };
}

fn printCPUState(self: *Self) !void {
    try self.stdout.print("\n", .{});

    const ix = self.mmu.read(u8, self.cpu.registers.pc);

    const details = switch (ix) {
        0xCB => instr.prefixedInstructionsDetails(self.mmu.read(u8, self.cpu.registers.pc + 1)),
        else => instr.instructionsDetails(ix),
    } orelse std.debug.panic(
        "Unknown '0x{X:0>2}' @ ${X:0>4}\n",
        .{ ix, self.cpu.registers.pc },
    );

    try self.stdout.print("${X:0>4}: {s}\n", .{ self.cpu.registers.pc, details.mnemonic });

    try self.stdout.print("Memory u8 = 0x{X:0>2}, i8 = {}, u16 = 0x{X:0>4}\n", .{
        self.mmu.read(u8, self.cpu.registers.pc + 1),
        self.mmu.read(i8, self.cpu.registers.pc + 1),
        self.mmu.read(u16, self.cpu.registers.pc + 1),
    });

    try self.stdout.print("Registers: A = {}, B = {}, C = {}, D = {}, E = {}, H = {}, L = {}\n", .{
        self.cpu.registers.af.ind.a,
        self.cpu.registers.bc.ind.b,
        self.cpu.registers.bc.ind.c,
        self.cpu.registers.de.ind.d,
        self.cpu.registers.de.ind.e,
        self.cpu.registers.hl.ind.h,
        self.cpu.registers.hl.ind.l,
    });

    try self.stdout.print("16-bit Registers: AF = {X:0>4}, BC = {X:0>4}, DE = {X:0>4}, HL = {X:0>4}\n", .{
        self.cpu.registers.af.all,
        self.cpu.registers.bc.all,
        self.cpu.registers.de.all,
        self.cpu.registers.hl.all,
    });

    try self.stdout.print("Flags: {}\n", .{self.cpu.registers.af.ind.f});

    try self.stdout.print("SP (0x{X:0>4}) points to: [ {X:0>4} {X:0>4} {X:0>4} {X:0>4} {X:0>4} ]\n", .{
        self.cpu.registers.sp,
        self.mmu.read(u16, self.cpu.registers.sp +% 0),
        self.mmu.read(u16, self.cpu.registers.sp +% 2),
        self.mmu.read(u16, self.cpu.registers.sp +% 4),
        self.mmu.read(u16, self.cpu.registers.sp +% 6),
        self.mmu.read(u16, self.cpu.registers.sp +% 8),
    });

    try self.stdout.print("PC (0x{X:0>4}) points to: [ {X:0>2} {X:0>2} {X:0>2} {X:0>2} {X:0>2} ]\n", .{
        self.cpu.registers.pc,
        self.mmu.read(u8, self.cpu.registers.pc +% 0),
        self.mmu.read(u8, self.cpu.registers.pc +% 1),
        self.mmu.read(u8, self.cpu.registers.pc +% 2),
        self.mmu.read(u8, self.cpu.registers.pc +% 3),
        self.mmu.read(u8, self.cpu.registers.pc +% 4),
    });

    try self.stdout.print("DE ({X:0>4}) points to: [ {X:0>4} {X:0>4} >{X:0>4}< {X:0>4} {X:0>4} {X:0>4} ]\n", .{
        self.cpu.registers.de.all,
        self.mmu.read(u8, self.cpu.registers.de.all -% 2),
        self.mmu.read(u8, self.cpu.registers.de.all -% 1),
        self.mmu.read(u8, self.cpu.registers.de.all +% 0),
        self.mmu.read(u8, self.cpu.registers.de.all +% 2),
        self.mmu.read(u8, self.cpu.registers.de.all +% 3),
        self.mmu.read(u8, self.cpu.registers.de.all +% 4),
    });

    try self.stdout.print("HL ({X:0>4}) points to: [ {X:0>4} {X:0>4} >{X:0>4}< {X:0>4} {X:0>4} {X:0>4} ]\n", .{
        self.cpu.registers.hl.all,
        self.mmu.read(u8, self.cpu.registers.hl.all -% 2),
        self.mmu.read(u8, self.cpu.registers.hl.all -% 1),
        self.mmu.read(u8, self.cpu.registers.hl.all +% 0),
        self.mmu.read(u8, self.cpu.registers.hl.all +% 2),
        self.mmu.read(u8, self.cpu.registers.hl.all +% 3),
        self.mmu.read(u8, self.cpu.registers.hl.all +% 4),
    });

    try self.stdout.print("\n", .{});
}

fn printPPUState(self: *Self) !void {
    try self.stdout.print("SCX = {X:0>2}, SCY = {X:0>2}, WX = {X:0>2}, WY = {X:0>2}\n", .{
        self.mmu.ioRegister(.scroll_x).*,
        self.mmu.ioRegister(.scroll_y).*,
        self.mmu.ioRegister(.window_x).*,
        self.mmu.ioRegister(.window_y).*,
    });

    try self.stdout.print("LY = {X:0>2}, LYC = {X:0>2}, BGPAL = {X:0>2}, OPAL0 = {X:0>2}, OPAL1 = {X:0>2}\n", .{
        self.mmu.ioRegister(.lcd_y_coord).*,
        self.mmu.ioRegister(.lcd_y_compare).*,
        self.mmu.ioRegister(.bg_palette).*,
        self.mmu.ioRegister(.spr_palette_0).*,
        self.mmu.ioRegister(.spr_palette_1).*,
    });
}

fn printTiledata(self: *Self, id: u16) !void {
    if (id <= 0xFF) {
        try self.stdout.print("{X:0>2} -- TILE: ", .{id});
    } else {
        try self.stdout.print("-- {X:0>2} TILE: ", .{id - 0x80});
    }

    for (std.mem.zeroes([16]void)) |_, offset| {
        try self.stdout.print("{X:0>2} ", .{self.mmu.video_ram[16 * id + offset]});
    }

    try self.stdout.print("\n", .{});
}

fn printTilemap(self: *Self, tilemap: u1) !void {
    const addr: u16 = switch (tilemap) {
        0 => 0x1800,
        1 => 0x1C00,
    };

    try self.stdout.print("TILEMAP ({X:0>4}) {} = (", .{ addr, tilemap });

    for (self.mmu.video_ram[addr .. addr + 0x400]) |tid, idx| {
        if (idx % 32 == 0) try self.stdout.print("\n\t", .{});
        try self.stdout.print("{X:0>2} ", .{tid});
    }

    try self.stdout.print(")\n", .{});
}

const Command = struct {
    raw_input: []u8,
    arguments: std.ArrayList([]const u8),
    command: enum {
        run,
        next,
        quit,
        show,
        print,
        memory,
        initwin,
        unknown,
        breakpoint,
    },
};

fn parseCommand(self: *Self) !Command {
    _ = try self.stdout.write("$ ");

    var input = (try self.stdin.readUntilDelimiterOrEofAlloc(self.allocator, '\n', 50)) orelse "";

    var command = Command{
        .raw_input = input,
        .arguments = std.ArrayList([]const u8).init(self.allocator),
        .command = .unknown,
    };

    var spliterator = std.mem.split(u8, input, " ");

    while (spliterator.next()) |arg| {
        if (command.command == .unknown) {
            command.command = if (std.mem.eql(u8, arg, "run")) @TypeOf(command.command).run //
            else if (std.mem.eql(u8, arg, "print")) @TypeOf(command.command).print //
            else if (std.mem.eql(u8, arg, "break")) @TypeOf(command.command).breakpoint //
            else if (std.mem.eql(u8, arg, "wind")) @TypeOf(command.command).initwin //
            else if (std.mem.eql(u8, arg, "quit")) @TypeOf(command.command).quit //
            else if (std.mem.eql(u8, arg, "show")) @TypeOf(command.command).show //
            else if (std.mem.eql(u8, arg, "memo")) @TypeOf(command.command).memory //
            else if (arg.len == 0 or std.mem.eql(u8, arg, "next")) @TypeOf(command.command).next //
            else break;
        } else {
            try command.arguments.append(arg);
        }
    }

    return command;
}

fn freeCommand(self: *Self, command: *Command) void {
    command.arguments.clearAndFree();
    self.allocator.free(command.raw_input);
}

fn executeCommand(self: *Self, command: *Command) !void {
    switch (command.command) {
        .next => {},

        .run => self.broke = false,

        .quit => self.quit = true,

        .unknown => try self.stdout.print("Unknown command!\n", .{}),

        .initwin => {
            self.window = try Window.init(self.allocator, .{});
            try self.window.?.setTitle(self.mmu.gameTitle());
        },

        .memory => {
            const addr_mask = ~@as(u16, 0xF);
            const arg = try std.fmt.parseUnsigned(u16, command.arguments.pop(), 0);
            const addr = arg & addr_mask;

            try self.stdout.print("MEMORY 0x{X:0>4}: ", .{addr});

            for (std.mem.zeroes([0x10]void)) |_, offset| {
                const byte = self.mmu.read(u8, addr + @intCast(u16, offset));
                try self.stdout.print("{X:0>2} ", .{byte});
            }

            try self.stdout.writeByte('\n');
        },

        .breakpoint => {
            const addr = try std.fmt.parseUnsigned(u16, command.arguments.pop(), 0);
            try self.breakpoints.put(addr, void{});
        },

        .show => {
            const arg = command.arguments.pop();

            if (std.mem.eql(u8, arg, "cpu")) {
                try self.printCPUState();
            } else if (std.mem.eql(u8, arg, "ppu")) {
                try self.printPPUState();
            } else {
                try self.stdout.print("Unknown '{s}' to print.", .{arg});
            }
        },

        .print => {
            const arg = command.arguments.pop();

            if (std.mem.eql(u8, arg, "cpu")) {
                self.print_states.cpu_state = true;
            } else if (std.mem.eql(u8, arg, "ppu")) {
                self.print_states.ppu_state = true;
            } else {
                try self.stdout.print("Unknown '{s}' to print.", .{arg});
            }
        },
    }
}

fn pauseForCommands(self: *Self) !void {
    while (true) {
        var command = try self.parseCommand();
        try self.executeCommand(&command);
        self.freeCommand(&command);

        switch (command.command) {
            .run, .next, .quit => break,
            else => {},
        }
    }
}

pub fn debug(self: *Self) !void {
    try self.stdout.print("Loaded ROM: '{s}' ({}kB)\n", .{
        self.mmu.gameTitle(),
        self.mmu.cartridge.len / 1_024,
    });

    try self.pauseForCommands();

    while (!self.quit) {
        const clocks = self.cpu.step();
        const rendering = self.ppu.step(clocks);

        if (self.breakpoints.contains(self.cpu.registers.pc)) {
            try self.stdout.print("Hit breakpoint {X:0>4}.\n", .{self.cpu.registers.pc});
            self.broke = true;
        }

        if (self.print_states.cpu_state) try self.printCPUState();
        if (self.print_states.ppu_state) try self.printPPUState();

        var tiledataIterator = self.print_states.tiledata.keyIterator();
        while (tiledataIterator.next()) |id| try self.printTiledata(id.*);

        var tilemapIterator = self.print_states.tilemap.keyIterator();
        while (tilemapIterator.next()) |map| try self.printTilemap(map.*);

        if (self.broke) {
            try self.pauseForCommands();
        }

        if (rendering) continue;

        if (self.window) |*win| {
            const draw = win.drawBegin();
            self.ppu.blit(draw.screen);
            win.drawFinish();
        }
    }
}
