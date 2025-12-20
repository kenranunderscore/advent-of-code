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

/// Ok, so I hated this part immediately upon reading it, as it invalidated
/// basically everything about my previous solution. So behold the biggest hack
/// I've done in Zig so far.
fn part2(alloc: std.mem.Allocator) !u64 {
    const f = try std.fs.cwd().openFile("input/day6", .{ .mode = .read_only });
    defer f.close();

    var line_writer = std.io.Writer.Allocating.init(alloc);
    defer line_writer.deinit();

    var buf: [1000]u8 = undefined;
    var reader = f.reader(&buf);

    var lines = std.ArrayList([]u8).empty;
    defer {
        for (lines.items) |line| alloc.free(line);
        lines.deinit(alloc);
    }

    var ops: []u8 = undefined;
    defer alloc.free(ops);

    // Collect all the lines
    while (true) {
        var eof = false;
        _ = reader.interface.streamDelimiter(&line_writer.writer, '\n') catch |err| switch (err) {
            error.EndOfStream => eof = true,
            else => return err,
        };

        const line = try alloc.dupe(u8, line_writer.written());
        line_writer.clearRetainingCapacity();
        if (!eof) {
            try lines.append(alloc, line);
            _ = reader.interface.toss(1);
        } else {
            ops = line;
            break;
        }
    }

    // Build up columns out of the lines
    const line_length = lines.items[0].len;
    var columns = try std.ArrayList([]const u8).initCapacity(alloc, line_length);
    defer {
        for (columns.items) |col| alloc.free(col);
        columns.deinit(alloc);
    }

    for (0..line_length) |x| {
        var col = try alloc.alloc(u8, lines.items.len);
        for (0..lines.items.len) |y| {
            col[y] = lines.items[y][x];
        }
        try columns.append(alloc, col);
    }

    // Go over it *again* and calculate the sums/products and the end result
    var result: u64 = 0;
    var group_result: ?u64 = null;
    var current_op: ?u8 = null;

    for (0..line_length + 1) |x| {
        if (x == line_length or std.mem.allEqual(u8, columns.items[x], ' ')) {
            result += group_result.?;
            current_op = null;
            continue;
        }

        const col = std.mem.trim(u8, columns.items[x], &[_]u8{' '});
        if (current_op == null) {
            current_op = ops[x];
            group_result = if (current_op == '+') 0 else 1;
        }

        const number = try std.fmt.parseInt(u64, col, 10);
        group_result = if (current_op.? == '+')
            group_result.? + number
        else
            group_result.? * number;
    }

    return result;
}

test "test input part 1" {
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

test "part 1" {
    const a = std.testing.allocator;
    var ctx = Context.init(a);
    defer ctx.deinit();

    try util.processFile(a, "input/day6", &ctx, callback, '\n');
    try std.testing.expectEqual(5361735137219, part1(ctx));
}

test "part 2" {
    try std.testing.expectEqual(11744693538946, try part2(std.testing.allocator));
}
