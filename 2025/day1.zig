const std = @import("std");
const util = @import("util.zig");

const Context = struct {
    pos: u16,
    count_part1: usize,
    count_part2: usize,
};

fn rotateDial(pos: u16, offset: i16) struct { u16, usize } {
    const p = @as(i32, pos);
    const o = @as(i32, offset);
    const new_pos: u16 = @intCast(@mod(p + o, 100));

    var hits: i32 = @divFloor(p + o, 100);
    if (offset < 0) {
        if (pos == 0 and new_pos != 0) {
            hits += 1;
        } else if (pos != 0 and new_pos == 0) {
            hits -= 1;
        }
    }

    return .{
        new_pos,
        @intCast(@abs(hits)),
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

fn run(ctx: *Context) !void {
    var gpa = std.heap.DebugAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    try util.processFile(
        alloc,
        "input/day1",
        ctx,
        countZeroesCallback,
    );
}

test "example input" {
    const lines = &[_][]const u8{
        "L68",
        "L30",
        "R48",
        "L5",
        "R60",
        "L55",
        "L1",
        "L99",
        "R14",
        "L82",
    };
    var ctx = Context{ .pos = 50, .count_part1 = 0, .count_part2 = 0 };
    try util.processLines(lines, &ctx, countZeroesCallback);
    try std.testing.expectEqual(32, ctx.pos);
    try std.testing.expectEqual(6, ctx.count_part2);
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
    try run(&ctx);
    try std.testing.expectEqual(1147, ctx.count_part1);
    try std.testing.expectEqual(6789, ctx.count_part2);
}
