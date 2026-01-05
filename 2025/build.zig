const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    var days = std.ArrayList(usize).empty;
    defer days.deinit(b.allocator);

    const src_dir = try std.fs.cwd().openDir("src", .{ .iterate = true });
    var iter = src_dir.iterate();
    while (try iter.next()) |entry| {
        if (entry.kind == .file and
            std.mem.startsWith(u8, entry.name, "day") and
            std.mem.endsWith(u8, entry.name, ".zig"))
        {
            const day = try std.fmt.parseInt(usize, entry.name[3 .. entry.name.len - 4], 10);
            try days.append(b.allocator, day);
        }
    }

    const test_all = b.step("test", "Run tests for all AoC days");
    for (days.items) |day| {
        const day_name = try std.fmt.allocPrint(b.allocator, "day{d}", .{day});
        const path = try std.fmt.allocPrint(b.allocator, "src/{s}.zig", .{day_name});
        const tests = b.addTest(.{
            .root_module = b.createModule(.{
                .root_source_file = b.path(path),
                .target = target,
                .optimize = optimize,
            }),
        });

        const day_step = b.step(
            day_name,
            try std.fmt.allocPrint(b.allocator, "Run tests for day {d}", .{day}),
        );
        const run_day_tests = b.addRunArtifact(tests);
        day_step.dependOn(&run_day_tests.step);
        test_all.dependOn(&run_day_tests.step);
    }

    const day9_mod = b.addModule("day9mod", .{
        .root_source_file = b.path("src/day9.zig"),
        .target = target,
        .optimize = optimize,
    });
    const day9 = b.addExecutable(.{
        .name = "day9",
        .root_module = day9_mod,
    });
    const day9_step = b.step("day9run", "Run day 9");
    const day9_run_cmd = b.addRunArtifact(day9);
    day9_run_cmd.step.dependOn(b.getInstallStep());
    day9_step.dependOn(&day9_run_cmd.step);

    const day9_check = b.addExecutable(.{
        .name = "day9check",
        .root_module = day9_mod,
    });
    const check = b.step("check", "Check whether the component compiles");
    check.dependOn(&day9_check.step);
}
