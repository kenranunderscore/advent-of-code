const std = @import("std");
const util = @import("util.zig");

const Context = struct {
    sum_part1: usize = 0,
    sum_part2: usize = 0,
};

fn findMax(line: []const u8) !struct { u8, usize } {
    var max: u8 = 0;
    var imax: usize = 0;
    for (0..line.len) |i| {
        const n = try std.fmt.parseInt(u8, line[i .. i + 1], 10);
        if (n > max) {
            max = n;
            imax = i;
        }
    }

    return .{ max, imax };
}

fn maxJoltage(line: []const u8, digits: usize) !u64 {
    if (digits == 0)
        return 0;
    const end = line.len - digits + 1;
    const max, const imax = try findMax(line[0..end]);
    const rest = line[imax + 1 ..];
    return try std.math.powi(u64, 10, digits - 1) * max +
        try maxJoltage(rest, digits - 1);
}

fn callback(line: []const u8, ctx: *Context) !void {
    if (line.len == 0) return;
    ctx.sum_part1 += @intCast(try maxJoltage(line, 2));
    ctx.sum_part2 += @intCast(try maxJoltage(line, 12));
}

test "maxJoltage" {
    const expectEqual = std.testing.expectEqual;
    try expectEqual(98, maxJoltage("987654321111111", 2));
    try expectEqual(34, maxJoltage("1234", 2));
    try expectEqual(43, maxJoltage("1423", 2));
    try expectEqual(91, maxJoltage("900000001", 2));
    try expectEqual(434234234278, maxJoltage("234234234234278", 12));
    try expectEqual(888911112111, maxJoltage("818181911112111", 12));
}

test {
    var ctx = Context{};
    try util.processFile(
        std.testing.allocator,
        "input/day3",
        &ctx,
        callback,
        '\n',
    );
    try std.testing.expectEqual(16_854, ctx.sum_part1);
    try std.testing.expectEqual(167_526_011_932_478, ctx.sum_part2);
}
