const std = @import("std");
const util = @import("util.zig");

const P = struct {
    x: u32,
    y: u32,
};

const Pair = struct {
    p: P,
    q: P,

    fn area(self: Pair) u64 {
        return (@abs(@as(i33, self.p.x) - @as(i33, self.q.x)) + 1) *
            (@abs(@as(i33, self.p.y) - @as(i33, self.q.y)) + 1);
    }
};

fn parsePoint(line: []const u8) !P {
    var iter = std.mem.tokenizeScalar(u8, line, ',');
    return P{
        .x = try std.fmt.parseInt(u32, iter.next().?, 10),
        .y = try std.fmt.parseInt(u32, iter.next().?, 10),
    };
}

fn getPairs(
    alloc: std.mem.Allocator,
    points: []P,
) !std.ArrayList(Pair) {
    const n_pairs = @divExact(points.len * (points.len - 1), 2);

    var pairs: std.ArrayList(Pair) = try .initCapacity(alloc, n_pairs);

    for (0..points.len - 1) |i| {
        for (i + 1..points.len) |j| {
            try pairs.append(
                alloc,
                Pair{ .p = points[i], .q = points[j] },
            );
        }
    }

    return pairs;
}

pub fn main() !void {
    var alloc = std.heap.DebugAllocator(.{}).init;
    defer std.debug.assert(alloc.deinit() == .ok);

    const gpa = alloc.allocator();

    const file = try std.fs.cwd().openFile("input/day9", .{ .mode = .read_only });
    const stat = try file.stat();
    const input = try file.readToEndAlloc(gpa, stat.size);
    defer gpa.free(input);

    var points: std.ArrayList(P) = .empty;
    defer points.deinit(gpa);

    var iter = std.mem.tokenizeScalar(u8, input, '\n');
    while (iter.next()) |line| {
        try points.append(gpa, try parsePoint(line));
    }

    var pairs = try getPairs(gpa, points.items);
    defer pairs.deinit(gpa);

    var max: u64 = 0;
    var rect: Pair = undefined;
    for (pairs.items) |pair| {
        if (pair.area() > max) {
            rect = pair;
            max = pair.area();
        }
    }

    std.debug.print("{any}\n", .{rect});
    std.debug.print("{d}\n", .{max});
}
