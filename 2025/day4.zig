const std = @import("std");
const util = @import("util.zig");

fn countNeighbors(
    lines: std.ArrayList([]const u8),
    x: usize,
    y: usize,
) usize {
    var count: usize = 0;
    for (0..3) |i| {
        for (0..3) |j| {
            if (x + i == 0 or y + j == 0) continue;
            const x_test = x + i - 1;
            const y_test = y + j - 1;
            if (x == x_test and y == y_test or
                x_test >= lines.items[0].len or
                y_test >= lines.items.len)
                continue;
            count += if (lines.items[x_test][y_test] == '@') 1 else 0;
        }
    }
    return count;
}

fn part1(lines: std.ArrayList([]const u8)) !usize {
    var count: usize = 0;
    for (0..lines.items[0].len) |x| {
        for (0..lines.items.len) |y| {
            if (lines.items[x][y] == '@' and countNeighbors(lines, x, y) < 4) {
                count += 1;
            }
        }
    }
    return count;
}

test "countNeighbors" {
    const a = std.testing.allocator;
    var lines = std.ArrayList([]const u8).empty;
    defer lines.deinit(a);
    try lines.append(a, "@.@");
    try lines.append(a, ".@@");
    try lines.append(a, "@..");

    try std.testing.expectEqual(4, countNeighbors(lines, 1, 1));
    try std.testing.expectEqual(1, countNeighbors(lines, 0, 0));
}

test {
    const input = try std.fs.cwd().openFile(
        "input/day4",
        .{ .mode = .read_only },
    );
    defer input.close();

    const stat = try input.stat();
    const a = std.testing.allocator;
    const buf = try input.readToEndAlloc(a, stat.size);
    defer a.free(buf);

    var tokens = std.mem.tokenizeScalar(u8, buf, '\n');
    var lines = try std.ArrayList([]const u8).initCapacity(a, stat.size);
    defer lines.deinit(a);
    while (tokens.next()) |line| {
        try lines.append(a, line);
    }

    const result = try part1(lines);
    try std.testing.expectEqual(1460, result);
}
