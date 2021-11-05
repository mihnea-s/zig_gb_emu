const std = @import("std");

const gl = @import("zgl");
const glfw = @import("zglfw");

const WindowConfig = struct {
    width: usize = 160,
    height: usize = 144,
    vsync: bool = true,
    window_width: usize = 640,
    window_height: usize = 580,
    window_title: [:0]const u8 = "CHIP-8",
};

fn compileShader(source: []const u8, stage: gl.ShaderType) !gl.Shader {
    const shader = gl.createShader(stage);

    gl.shaderSource(shader, 1, &[_][]const u8{source});
    gl.compileShader(shader);

    const log = try gl.getShaderInfoLog(shader, std.heap.c_allocator);
    if (log.len > 0) {
        std.debug.print("shader compile error: {s}", .{log});
        return error.CompilationError;
    }

    return shader;
}

fn compileProgram(vert: gl.Shader, frag: gl.Shader) !gl.Program {
    const program = gl.createProgram();

    gl.attachShader(program, vert);
    gl.attachShader(program, frag);
    gl.linkProgram(program);

    const log = try gl.getProgramInfoLog(program, std.heap.c_allocator);
    if (log.len > 0) {
        std.debug.print("program link error: {s}", .{log});
        return error.LinkError;
    }

    return program;
}

pub const Window = struct {
    const Self = @This();

    window: *glfw.Window,
    program: gl.Program,

    screen_texture: gl.Texture,
    screen_width: usize,
    screen_height: usize,
    pixel_buffer: gl.Buffer,

    vao: gl.VertexArray,
    vertex_buffer: gl.Buffer,
    vertex_count: usize,

    pub fn init(config: WindowConfig) !Self {
        try glfw.init();

        // At least version 3.3
        glfw.windowHint(.ContextVersionMajor, 3);
        glfw.windowHint(.ContextVersionMinor, 3);

        // Use core profile (instead of legacy)
        const core_profile = glfw.GLProfileAttribute.OpenglCoreProfile;
        glfw.windowHint(.OpenGLProfile, @enumToInt(core_profile));

        const window = try glfw.createWindow(
            @intCast(c_int, config.window_width),
            @intCast(c_int, config.window_height),
            config.window_title,
            null,
            null,
        );

        // Initialise GL context
        glfw.makeContextCurrent(window);
        gl.clearColor(0.0, 0.0, 0.0, 1.0);

        if (!config.vsync) glfw.swapInterval(0);

        const resizeCallback = struct {
            fn function(_: *glfw.Window, new_width: c_int, new_height: c_int) callconv(.C) void {
                gl.viewport(0, 0, @intCast(usize, new_width), @intCast(usize, new_height));
            }
        }.function;

        _ = glfw.setWindowSizeCallback(window, resizeCallback);

        const vert = try compileShader(@embedFile("./shaders/vert.glsl"), .vertex);
        const frag = try compileShader(@embedFile("./shaders/frag.glsl"), .fragment);

        const program = try compileProgram(vert, frag);

        gl.deleteShader(vert);
        gl.deleteShader(frag);

        const vert_data = [_]f32{
            -1.0, 1.0,
            1.0,  1.0,
            -1.0, -1.0,

            1.0,  1.0,
            1.0,  -1.0,
            -1.0, -1.0,
        };

        const buffer = gl.createBuffer();
        const vao = gl.createVertexArray();
        gl.bindVertexArray(vao);
        gl.bindBuffer(buffer, .array_buffer);
        gl.enableVertexAttribArray(0);
        gl.vertexAttribPointer(0, 2, .float, false, 2 * @sizeOf(f32), 0);
        gl.bufferData(.array_buffer, f32, &vert_data, .static_draw);

        const screen = try std.heap.page_allocator.alloc(u8, config.width * config.height);
        defer std.heap.page_allocator.free(screen);

        const pbo = gl.createBuffer();
        gl.bindBuffer(pbo, .pixel_unpack_buffer);
        gl.bufferData(.pixel_unpack_buffer, u8, screen, .dynamic_draw);

        const tex = gl.createTexture(.@"2d");
        gl.bindTexture(tex, .@"2d");
        gl.texParameter(.@"2d", .min_filter, .nearest);
        gl.texParameter(.@"2d", .mag_filter, .nearest);
        gl.texParameter(.@"2d", .wrap_s, .clamp_to_edge);
        gl.texParameter(.@"2d", .wrap_t, .clamp_to_edge);
        gl.textureImage2D(.@"2d", 0, .rgb, config.width, config.height, .red, .unsigned_byte, null);

        const context = .{
            .window = window,
            .program = program,

            .screen_texture = tex,
            .screen_width = config.width,
            .screen_height = config.height,
            .pixel_buffer = pbo,

            .vao = vao,
            .vertex_buffer = buffer,
            .vertex_count = vert_data.len,
        };

        return context;
    }

    pub fn deinit(self: *Self) void {
        gl.deleteVertexArray(self.vao);
        gl.deleteBuffers(&.{ self.vertex_buffer, self.pixel_buffer });
        gl.deleteTexture(self.screen_texture);
        gl.deleteProgram(self.program);

        glfw.destroyWindow(self.window);
        glfw.terminate();
    }

    pub fn alive(self: *Self) bool {
        return !glfw.windowShouldClose(self.window);
    }

    pub const Key = glfw.Key;

    pub fn keyPressed(self: *Self, key: Key) bool {
        const state = glfw.getKey(self.window, key);
        return state == .Press or state == .Repeat;
    }

    pub fn keyReleased(self: *Self, key: Key) bool {
        return glfw.getKey(self.window, key) == .Release;
    }

    pub const DrawContext = struct {
        width: usize,
        height: usize,
        screen: []u8,
    };

    pub fn drawBegin(self: *Self) DrawContext {
        glfw.pollEvents();
        gl.clear(.{ .color = true });
        gl.bindBuffer(self.pixel_buffer, .pixel_unpack_buffer);

        var pixel_buffer = gl.mapBuffer(.pixel_unpack_buffer, u8, .write_only);
        var screen = pixel_buffer[0..(self.screen_width * self.screen_height)];

        return .{
            .screen = screen,
            .width = self.screen_width,
            .height = self.screen_height,
        };
    }

    pub fn drawFinish(self: *Self) void {
        gl.bindBuffer(self.pixel_buffer, .pixel_unpack_buffer);
        _ = (gl.unmapBuffer(.pixel_unpack_buffer));

        gl.textureImage2D(
            .@"2d", // Texture target
            0, // Mipmap level
            .rgb, // Internal format
            self.screen_width, // Width
            self.screen_height, //Height
            .red, // Pixel data format
            .unsigned_byte, // Pixel data type
            null, // Data (will use pixel buffer)
        );

        gl.useProgram(self.program);
        gl.bindVertexArray(self.vao);
        gl.bindTexture(self.screen_texture, .@"2d");
        gl.drawArrays(.triangles, 0, self.vertex_count);

        glfw.swapBuffers(self.window);
    }
};
