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

test {
    const a = std.testing.allocator;
    var ctx = Context{ .allocator = a };
    defer ctx.deinit();
    try util.processFile(a, "input/day7", &ctx, callback, '\n');
    const res = try part1(&ctx);
    // for (ctx.lines.items) |l| std.debug.print("{s}\n", .{l});
    try std.testing.expectEqual(1675, res);
}
