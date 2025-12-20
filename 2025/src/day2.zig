const std = @import("std");
const util = @import("util.zig");
const t = std.testing;

const Context = struct {
    sum: usize = 0,
};

fn parseRange(input: []const u8) !struct { u64, u64 } {
    var it = std.mem.splitScalar(u8, input, '-');
    const start = try std.fmt.parseInt(u64, it.next().?, 10);
    const end = try std.fmt.parseInt(u64, it.next().?, 10);
    return .{ start, end };
}

fn digits(n: u64) usize {
    return std.math.log10_int(n) + 1;
}

fn valid(n: u64) !bool {
    const d = digits(n);
    if (@mod(d, 2) == 0) {
        const h = try std.math.powi(u64, 10, @divExact(d, 2));
        const left = @divFloor(n, h);
        const right = @mod(n, h);
        return left != right;
    }

    return true;
}

fn validForPart2(n: u64) !bool {
    const d = digits(n);
    var group_size: usize = 1;
    while (group_size < d) {
        var first: u64 = undefined;
        var found = true;
        if (@mod(d, group_size) == 0) {
            for (0..@divExact(d, group_size)) |i| {
                const pot_mod = try std.math.powi(u64, 10, d - i * group_size);
                const pot_div = try std.math.powi(u64, 10, d - (i + 1) * group_size);
                const group = @divFloor(@mod(n, pot_mod), pot_div);
                if (i == 0) {
                    first = group;
                } else if (first != group) {
                    found = false;
                }
            }
            if (found) return false;
        }

        group_size += 1;
    }

    return true;
}

fn callback(line: []const u8, ctx: *Context) !void {
    const s, const e = try parseRange(line);
    var i = s;
    while (i <= e) {
        if (!try valid(i)) ctx.sum += i;
        i += 1;
    }
}

fn callbackPart2(line: []const u8, ctx: *Context) !void {
    const s, const e = try parseRange(line);
    var i = s;
    while (i <= e) {
        if (!try validForPart2(i)) ctx.sum += i;
        i += 1;
    }
}

test "parseRange" {
    const start, const end = try parseRange("13-1234");
    try t.expectEqual(13, start);
    try t.expectEqual(1234, end);
}

test "digits" {
    const n: u64 = 123123;
    try t.expectEqual(6, digits(n));
}

test "invalid" {
    try t.expect(!try valid(11));
    try t.expect(!try valid(123123));
    try t.expect(try valid(4546));
    try t.expect(try valid(5));
}

test {
    var ctx = Context{};
    try util.processFile(
        std.testing.allocator,
        "input/day2",
        &ctx,
        callback,
        ',',
    );

    try std.testing.expectEqual(55916882972, ctx.sum);
}

test {
    var ctx = Context{};
    try util.processFile(
        std.testing.allocator,
        "input/day2_test_input",
        &ctx,
        callback,
        ',',
    );

    try std.testing.expectEqual(1227775554, ctx.sum);
}

test "valid part 2" {
    try std.testing.expect(try validForPart2(12_345_678));
    try std.testing.expect(!try validForPart2(11));
    try std.testing.expect(!try validForPart2(151515));
    try std.testing.expect(try validForPart2(112233));
    try std.testing.expect(!try validForPart2(2121212121));
}

test {
    var ctx = Context{};
    try util.processFile(
        std.testing.allocator,
        "input/day2",
        &ctx,
        callbackPart2,
        ',',
    );

    try std.testing.expectEqual(76169125915, ctx.sum);
}
