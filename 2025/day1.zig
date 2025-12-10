const std = @import("std");
const util = @import("util.zig");

const Dir = enum {
    left,
    right,
};

fn rotateDial(pos: i16, dir: Dir, offset: i16) i16 {
    if (dir == .left) {
        return pos - offset;
    } else {
        return pos + offset;
    }
}

const Context = struct {
    pos: i16,
    count: usize,
};

fn countZeroesCallback(line: []const u8, ctx: *Context) !void {
    const offset = try std.fmt.parseInt(i16, line[1..], 10);
    const dir: Dir = if (line[0] == 'L') .left else .right;
    const new_pos = rotateDial(ctx.pos, dir, offset);
    ctx.pos = new_pos;

    if (@mod(new_pos, 100) == 0) {
        ctx.count += 1;
    }
}

fn countZeroes() !usize {
    var gpa = std.heap.DebugAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    var ctx = Context{ .pos = 50, .count = 0 };
    try util.processLines(alloc, "input/day1", &ctx, countZeroesCallback);

    return ctx.count;
}

test "part 1" {
    const result = countZeroes();
    try std.testing.expectEqual(1147, result);
}
