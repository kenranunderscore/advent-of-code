const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const test_all = b.step("test", "Run tests for all AoC days");
    for (1..7) |day| {
        const day_name = try std.fmt.allocPrint(b.allocator, "day{d}", .{day});
        const path = try std.fmt.allocPrint(b.allocator, "{s}.zig", .{day_name});
        const tests = b.addTest(.{
            .root_module = b.createModule(.{
                .root_source_file = b.path(path),
                .target = target,
                .optimize = optimize,
            }),
        });

        const day_step = b.step(
            day_name,
            try std.fmt.allocPrint(b.allocator, "Run test for day {d}", .{day}),
        );
        const run_day_tests = b.addRunArtifact(tests);
        day_step.dependOn(&run_day_tests.step);
        test_all.dependOn(&run_day_tests.step);
    }
}
