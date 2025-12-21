const std = @import("std");
const util = @import("util.zig");

const Context = struct {
    allocator: std.mem.Allocator,
    lines: std.ArrayList([]u8) = std.ArrayList([]u8).empty,

    fn deinit(self: *@This()) void {
        for (self.lines.items) |line| self.allocator.free(line);
        self.lines.deinit(self.allocator);
    }
};

fn callback(line: []const u8, ctx: *Context) !void {
    try ctx.lines.append(ctx.allocator, try ctx.allocator.dupe(u8, line));
}

fn part1(ctx: *Context) !usize {
    var result: usize = 0;
    const line_length = ctx.lines.items[0].len;
    for (1..ctx.lines.items.len) |y| {
        const above = ctx.lines.items[y - 1];
        var line = ctx.lines.items[y];
        for (0..line_length) |x| {
            if (above[x] == '|' or above[x] == 'S') {
                if (line[x] == '.') line[x] = '|';

                if (line[x] == '^') {
                    var split = false;
                    if (x > 0 and line[x - 1] == '.') {
                        line[x - 1] = '|';
                        split = true;
                    }
                    if (x < line_length - 1 and line[x + 1] == '.') {
                        line[x + 1] = '|';
                        split = true;
                    }
                    if (split) result += 1;
                }
            }
        }
    }
    return result;
}

const Pos = struct {
    x: usize,
    remaining: usize,
};

fn timelines(
    lines: []const []const u8,
    x: usize,
    lookup: *std.AutoHashMap(Pos, usize),
) !usize {
    if (lines.len == 0)
        return 1;

    if (lookup.get(Pos{ .x = x, .remaining = lines.len })) |n|
        return n;

    const c = lines[0][x];
    const remaining = lines[1..];

    if (c == '^') {
        return (if (x > 0)
            try timelines(remaining, x - 1, lookup)
        else
            0) +
            (if (x < lines[0].len - 1)
                try timelines(remaining, x + 1, lookup)
            else
                0);
    }

    const res = try timelines(remaining, x, lookup);
    try lookup.put(Pos{ .x = x, .remaining = lines.len }, res);
    return res;
}

fn part2(
    alloc: std.mem.Allocator,
    lines: []const []const u8,
) !usize {
    var lookup = std.AutoHashMap(Pos, usize).init(alloc);
    defer lookup.deinit();

    for (0..lines[0].len) |x| {
        if (lines[0][x] == 'S') {
            return timelines(lines[1..], x, &lookup);
        }
    }

    return error.NoStartingPositionFound;
}

test "part 1" {
    const a = std.testing.allocator;
    var ctx = Context{ .allocator = a };
    defer ctx.deinit();

    try util.processFile(a, "input/day7", &ctx, callback, '\n');

    try std.testing.expectEqual(1675, try part1(&ctx));
}

test "part 2" {
    const a = std.testing.allocator;
    var ctx = Context{ .allocator = a };
    defer ctx.deinit();

    try util.processFile(a, "input/day7", &ctx, callback, '\n');

    try std.testing.expectEqual(187987920774390, try part2(a, ctx.lines.items));
}
