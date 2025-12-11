const std = @import("std");
const util = @import("util.zig");

const Dir = enum {
    left,
    right,
};

const Context = struct {
    pos: u16,
    count_part1: usize,
    count_part2: usize,
};

fn rotateDial(pos: u16, offset: i16) struct { u16, usize } {
    const p = @as(i32, pos);
    const o = @as(i32, offset);
    const new_pos: u16 = @intCast(@mod(p + o, 100));

    var n: i32 = pos;
    var hits: usize = 0;
    for (0..@abs(offset)) |_| {
        n = @mod(if (offset < 0) (n - 1) else n + 1, 100);
        if (n == 0)
            hits += 1;
    }

    return .{
        new_pos,
        hits,
    };
}

fn countZeroesCallback(line: []const u8, ctx: *Context) !void {
    var offset = try std.fmt.parseInt(i16, line[1..], 10);
    if (line[0] == 'L') {
        offset *= -1;
    }
    const new_pos, const hits = rotateDial(ctx.pos, offset);

    if (new_pos == 0) {
        ctx.count_part1 += 1;
    }
    ctx.pos = new_pos;
    ctx.count_part2 += hits;
}

fn countZeroes(ctx: *Context) !void {
    var gpa = std.heap.DebugAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    try util.processLines(
        alloc,
        "input/day1",
        ctx,
        countZeroesCallback,
    );
}

test "given example" {
    const equal = std.testing.expectEqual;

    var p, var hits = rotateDial(50, -68);
    try equal(1, hits);
    try equal(82, p);

    p, hits = rotateDial(p, -30);
    try equal(0, hits);
    try equal(52, p);

    p, hits = rotateDial(p, 48);
    try equal(1, hits);
    try equal(0, p);

    p, hits = rotateDial(p, -5);
    try equal(0, hits);
    try equal(95, p);

    p, hits = rotateDial(p, 60);
    try equal(1, hits);
    try equal(55, p);

    p, hits = rotateDial(p, -55);
    try equal(1, hits);
    try equal(0, p);

    p, hits = rotateDial(p, -1);
    try equal(0, hits);
    try equal(99, p);

    p, hits = rotateDial(p, -99);
    try equal(1, hits);
    try equal(0, p);

    p, hits = rotateDial(p, 14);
    try equal(0, hits);
    try equal(14, p);

    p, hits = rotateDial(p, -82);
    try equal(1, hits);
    try equal(32, p);
}

test "special case: move left from 0" {
    const p, const hits = rotateDial(0, -5);
    try std.testing.expectEqual(0, hits);
    try std.testing.expectEqual(95, p);
}

test "multiple overflow to the right" {
    _, const hits = rotateDial(50, 890);
    try std.testing.expectEqual(9, hits);
}

test "more special cases" {
    _, var hits = rotateDial(3, -299);
    try std.testing.expectEqual(3, hits);

    _, hits = rotateDial(0, -299);
    try std.testing.expectEqual(2, hits);

    _, hits = rotateDial(0, -100);
    try std.testing.expectEqual(1, hits);

    _, hits = rotateDial(0, -500);
    try std.testing.expectEqual(5, hits);
}

test "part 1 and 2" {
    var ctx = Context{
        .pos = 50,
        .count_part1 = 0,
        .count_part2 = 0,
    };
    try countZeroes(&ctx);
    try std.testing.expectEqual(1147, ctx.count_part1);
    try std.testing.expectEqual(6789, ctx.count_part2);
}
