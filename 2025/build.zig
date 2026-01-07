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

    for (days.items) |day| {
        const day_name = try std.fmt.allocPrint(b.allocator, "day{d}", .{day});
        const path = try std.fmt.allocPrint(b.allocator, "src/{s}.zig", .{day_name});

        const mod = b.createModule(.{
            .root_source_file = b.path(path),
            .target = target,
            .optimize = optimize,
        });

        const exe = b.addExecutable(.{
            .name = day_name,
            .root_module = mod,
        });
        const run = b.addRunArtifact(exe);

        const tests = b.addTest(.{ .root_module = mod });
        const run_tests = b.addRunArtifact(tests);

        const step = b.step(
            day_name,
            try std.fmt.allocPrint(b.allocator, "Run day {d}", .{day}),
        );
        step.dependOn(&run.step);
        step.dependOn(&run_tests.step);
    }
}
