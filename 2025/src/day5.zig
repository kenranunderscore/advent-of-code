const std = @import("std");
const util = @import("util.zig");

const Range = struct {
    start: u64,
    end: u64,

    const Self = @This();

    fn contains(self: Self, n: u64) bool {
        return self.start <= n and n <= self.end;
    }

    fn leftOf(self: Self, other: Range) bool {
        return self.left < other.left;
    }
};

fn parseRange(line: []const u8) !Range {
    var it = std.mem.splitScalar(u8, line, '-');
    const start = try std.fmt.parseInt(u64, it.next().?, 10);
    const end = try std.fmt.parseInt(u64, it.next().?, 10);
    return Range{ .start = start, .end = end };
}

const Context = struct {
    found: bool = false,
    ranges: std.ArrayList(Range) = std.ArrayList(Range).empty,
    numbers: std.ArrayList(u64) = std.ArrayList(u64).empty,
    alloc: std.mem.Allocator,

    const Self = @This();

    fn init(alloc: std.mem.Allocator) Context {
        return Context{ .alloc = alloc };
    }

    fn deinit(self: *Self) void {
        self.ranges.deinit(self.alloc);
        self.numbers.deinit(self.alloc);
    }

    fn appendRange(self: *Self, range: Range) !void {
        try self.ranges.append(self.alloc, range);
    }

    fn appendNumber(self: *Self, n: u64) !void {
        try self.numbers.append(self.alloc, n);
    }
};

fn callback(line: []const u8, ctx: *Context) !void {
    if (!ctx.found and line.len > 0) {
        try ctx.appendRange(try parseRange(line));
        return;
    }
    if (!ctx.found and line.len == 0) {
        ctx.found = true;
        return;
    }
    try ctx.appendNumber(try std.fmt.parseInt(u64, line, 10));
}

fn part1(ranges: std.ArrayList(Range), numbers: std.ArrayList(u64)) usize {
    var sum: usize = 0;
    outer: for (numbers.items) |n| {
        for (ranges.items) |range| {
            if (n >= range.start and n <= range.end) {
                sum += 1;
                continue :outer;
            }
        }
    }
    return sum;
}

fn part2(_: std.mem.Allocator, ranges: *std.ArrayList(Range)) !usize {
    outer: while (true) {
        for (0..ranges.items.len) |i| {
            for (i + 1..ranges.items.len) |j| {
                const r1 = ranges.items[i];
                const r2 = ranges.items[j];

                if (r1.start >= r2.start and r1.end <= r2.end) {
                    _ = ranges.orderedRemove(i);
                    continue :outer;
                }

                if (r2.start >= r1.start and r2.end <= r1.end) {
                    _ = ranges.orderedRemove(j);
                    continue :outer;
                }

                if (r1.start <= r2.end and r2.start <= r1.end) {
                    ranges.items[i] = Range{
                        .start = @min(r1.start, r2.start),
                        .end = @max(r1.end, r2.end),
                    };
                    _ = ranges.orderedRemove(j);
                    continue :outer;
                }
            }
        }

        break;
    }

    var sum: usize = 0;
    for (ranges.items) |range| {
        sum += range.end - range.start + 1;
    }
    return sum;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer std.debug.assert(gpa.deinit() == .ok);
    const alloc = gpa.allocator();

    var ctx = Context.init(alloc);
    defer ctx.deinit();

    try util.processFile(
        alloc,
        "input/day5",
        &ctx,
        callback,
        '\n',
    );

    const part1_result = part1(ctx.ranges, ctx.numbers);
    std.debug.assert(617 == part1_result);

    const part2_result = try part2(alloc, &ctx.ranges);
    std.debug.assert(338258295736104 == part2_result);

    std.debug.print("Part 1: {d}\nPart 2: {d}\n", .{ part1_result, part2_result });
}
