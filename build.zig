const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    const exe = b.addExecutable("chipper", "src/main.zig");

    // Build options
    exe.setTarget(b.standardTargetOptions(.{}));
    exe.setBuildMode(b.standardReleaseOptions());

    // Dependency libraries
    exe.addPackagePath("zgl", "libs/zgl/zgl.zig");
    exe.addPackagePath("zglfw", "libs/zglfw/src/main.zig");

    exe.linkSystemLibrary("epoxy");
    exe.linkSystemLibrary("glfw");
    exe.linkLibC();
    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
