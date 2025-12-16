const std = @import("std");
const util = @import("util.zig");

const Context = struct {
    sum: usize = 0,
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

fn maxJoltage(line: []const u8) !u8 {
    const max, const imax = try findMax(line[0 .. line.len - 1]);
    const max2, _ = try findMax(line[imax + 1 ..]);
    return 10 * max + max2;
}

fn callback(line: []const u8, ctx: *Context) !void {
    ctx.sum += @intCast(try maxJoltage(line));
}

test "maxJoltage" {
    try std.testing.expectEqual(98, maxJoltage("987654321111111"));
    try std.testing.expectEqual(34, maxJoltage("1234"));
    try std.testing.expectEqual(43, maxJoltage("1423"));
    try std.testing.expectEqual(91, maxJoltage("900000001"));
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
    try std.testing.expectEqual(16_854, ctx.sum);
}
