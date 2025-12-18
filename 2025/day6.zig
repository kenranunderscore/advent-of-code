const std = @import("std");
const util = @import("util.zig");

const Context = struct {
    alloc: std.mem.Allocator,
    number_lines: std.ArrayList(std.ArrayList(u64)),
    ops: std.ArrayList(u8) = undefined,

    const Self = @This();

    fn init(alloc: std.mem.Allocator) Self {
        const number_lines = std.ArrayList(std.ArrayList(u64)).empty;
        return Self{
            .alloc = alloc,
            .number_lines = number_lines,
        };
    }

    fn deinit(self: *Self) void {
        for (self.number_lines.items) |*l| l.deinit(self.alloc);
        self.number_lines.deinit(self.alloc);
        self.ops.deinit(self.alloc);
    }

    fn addNumbers(self: *Self, line: std.ArrayList(u64)) !void {
        try self.number_lines.append(self.alloc, line);
    }
};

fn parseNumberLine(alloc: std.mem.Allocator, line: []const u8) !std.ArrayList(u64) {
    var numbers = std.ArrayList(u64).empty;
    var tokens = std.mem.tokenizeScalar(u8, line, ' ');
    while (tokens.next()) |s| {
        const num = try std.fmt.parseUnsigned(u64, s, 10);
        try numbers.append(alloc, num);
    }

    return numbers;
}

fn parseOperators(alloc: std.mem.Allocator, line: []const u8) !std.ArrayList(u8) {
    var ops = std.ArrayList(u8).empty;
    var tokens = std.mem.tokenizeScalar(u8, line, ' ');
    while (tokens.next()) |s| {
        try ops.append(alloc, s[0]);
    }
    return ops;
}

fn callback(line: []const u8, ctx: *Context) !void {
    const numbers = parseNumberLine(ctx.alloc, line) catch {
        ctx.ops = try parseOperators(ctx.alloc, line);
        return;
    };
    try ctx.addNumbers(numbers);
}

fn part1(ctx: Context) u64 {
    var res: u64 = 0;
    for (0..ctx.ops.items.len) |x| {
        const total = switch (ctx.ops.items[x]) {
            '*' => prod: {
                var product: u64 = 1;
                for (0..ctx.number_lines.items.len) |y|
                    product *= ctx.number_lines.items[y].items[x];
                break :prod product;
            },
            '+' => sum: {
                var sum: u64 = 0;
                for (0..ctx.number_lines.items.len) |y|
                    sum += ctx.number_lines.items[y].items[x];
                break :sum sum;
            },
            else => unreachable,
        };
        res += total;
    }
    return res;
}

test "test input" {
    const a = std.testing.allocator;
    var ctx = Context.init(a);
    defer ctx.deinit();

    const input = &[_][]const u8{
        "123 328  51 64 ",
        " 45 64  387 23 ",
        "  6 98  215 314",
        "*   +   *   +  ",
    };

    try util.processLines(input, &ctx, callback);
    try std.testing.expectEqual(4277556, part1(ctx));
}

test {
    const a = std.testing.allocator;
    var ctx = Context.init(a);
    defer ctx.deinit();

    try util.processFile(a, "input/day6", &ctx, callback, '\n');
    try std.testing.expectEqual(5361735137219, part1(ctx));
}
