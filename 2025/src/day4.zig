const std = @import("std");
const util = @import("util.zig");

fn countNeighbors(
    lines: std.ArrayList([]u8),
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

fn part1(lines: std.ArrayList([]u8)) !usize {
    var accessible: usize = 0;
    for (0..lines.items[0].len) |x| {
        for (0..lines.items.len) |y| {
            if (lines.items[x][y] == '@' and countNeighbors(lines, x, y) < 4) {
                accessible += 1;
            }
        }
    }
    return accessible;
}

fn part2(lines: std.ArrayList([]u8)) !usize {
    var removed: usize = 0;
    var x: usize = 0;
    var y: usize = 0;
    outer: while (true) {
        while (x < lines.items[0].len) : (x += 1) {
            while (y < lines.items.len) : (y += 1) {
                if (lines.items[x][y] == '@' and countNeighbors(lines, x, y) < 4) {
                    lines.items[x][y] = '.';
                    if (x > 0) x -= 1;
                    if (y > 0) y -= 1;
                    removed += 1;
                    continue :outer;
                }
            }
            y = 0;
        }
        break;
    }

    return removed;
}

test "countNeighbors" {
    const a = std.testing.allocator;
    var lines = std.ArrayList([]u8).empty;
    defer {
        for (lines.items) |item| a.free(item);
        lines.deinit(a);
    }

    try lines.append(a, try a.dupe(u8, "@.@"));
    try lines.append(a, try a.dupe(u8, ".@@"));
    try lines.append(a, try a.dupe(u8, "@.."));

    try std.testing.expectEqual(4, countNeighbors(lines, 1, 1));
    try std.testing.expectEqual(1, countNeighbors(lines, 0, 0));
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer std.debug.assert(gpa.deinit() == .ok);
    const alloc = gpa.allocator();

    const input = try std.fs.cwd().openFile(
        "input/day4",
        .{ .mode = .read_only },
    );
    defer input.close();

    const stat = try input.stat();
    const buf = try input.readToEndAlloc(alloc, stat.size);
    defer alloc.free(buf);

    var tokens = std.mem.tokenizeScalar(u8, buf, '\n');
    var lines = try std.ArrayList([]u8).initCapacity(alloc, stat.size);
    defer {
        for (lines.items) |item| alloc.free(item);
        lines.deinit(alloc);
    }

    while (tokens.next()) |line| {
        const owned = try alloc.dupe(u8, line);
        try lines.append(alloc, owned);
    }

    const part1_result = try part1(lines);
    std.debug.assert(1460 == part1_result);

    const part2_result = try part2(lines);
    std.debug.assert(9243 == part2_result);

    std.debug.print("Part 1: {d}\nPart 2: {d}\n", .{ part1_result, part2_result });
}
