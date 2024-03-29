const std = @import("std");
//const deps = @import("zig-tracy/deps.zig");

pub fn build(b: *std.build.Builder) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();

    const exe = b.addExecutable("piwo", "src/main.zig");
    exe.setTarget(target);
    exe.setBuildMode(mode);
    //deps.addAllTo(exe);
    exe.linkLibC();
    exe.linkSystemLibrary("tcc");
    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const exe_tests = b.addTest("src/main.zig");
    const lex_tests = b.addTest("src/lexer.zig");
    const par_tests = b.addTest("src/parser.zig");
    
    exe_tests.setTarget(target);
    exe_tests.setBuildMode(mode);

    lex_tests.setTarget(target);
    lex_tests.setBuildMode(mode);

    par_tests.setTarget(target);
    par_tests.setBuildMode(mode);
    
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&exe_tests.step);
    test_step.dependOn(&lex_tests.step);
    test_step.dependOn(&par_tests.step);
}
