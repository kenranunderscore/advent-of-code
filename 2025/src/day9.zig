const std = @import("std");
const util = @import("util.zig");

const P = struct {
    x: u32,
    y: u32,
};

const Line = struct {
    p: P,
    q: P,

    fn area(self: Line) u64 {
        return (@abs(@as(i33, self.p.x) - @as(i33, self.q.x)) + 1) *
            (@abs(@as(i33, self.p.y) - @as(i33, self.q.y)) + 1);
    }

    fn isVertical(self: Line) bool {
        return self.p.x == self.q.x;
    }

    fn isHorizontal(self: Line) bool {
        return self.p.y == self.q.y;
    }
};

fn width(rect: Line) u32 {
    return @max(rect.p.x, rect.q.x) - @min(rect.p.x, rect.q.x) + 1;
}

fn height(rect: Line) u32 {
    return @max(rect.p.y, rect.q.y) - @min(rect.p.y, rect.q.y) + 1;
}

fn isBetween(x: u32, a: u32, b: u32) bool {
    return a <= x and x <= b or b <= x and x <= a;
}

fn parsePoint(line: []const u8) !P {
    var iter = std.mem.tokenizeScalar(u8, line, ',');
    return P{
        .x = try std.fmt.parseInt(u32, iter.next().?, 10),
        .y = try std.fmt.parseInt(u32, iter.next().?, 10),
    };
}

fn getRects(
    alloc: std.mem.Allocator,
    points: []P,
) !std.ArrayList(Line) {
    const n_rects = @divExact(points.len * (points.len - 1), 2);

    var rects: std.ArrayList(Line) = try .initCapacity(alloc, n_rects);

    for (0..points.len - 1) |i| {
        for (i + 1..points.len) |j| {
            try rects.append(
                alloc,
                Line{ .p = points[i], .q = points[j] },
            );
        }
    }

    return rects;
}

pub fn part1(
    alloc: std.mem.Allocator,
    points: std.ArrayList(P),
) !void {
    var rects = try getRects(alloc, points.items);
    defer rects.deinit(alloc);

    var max: u64 = 0;
    var max_rect: Line = undefined;

    for (rects.items) |rect| {
        if (rect.area() > max) {
            max_rect = rect;
            max = rect.area();
        }
    }

    std.debug.print("Part 1: {d}\n", .{max});
}

fn containsPoint(points: std.ArrayList(P), p: P) bool {
    for (points.items) |q| {
        if (q.x == p.x and q.y == p.y) return true;
    }

    return false;
}

fn getEdges(
    alloc: std.mem.Allocator,
    points: std.ArrayList(P),
) !std.ArrayList(Line) {
    var edges: std.ArrayList(Line) = .empty;

    for (0..points.items.len - 1) |i| {
        const edge = Line{ .p = points.items[i], .q = points.items[i + 1] };
        try edges.append(alloc, edge);
    }

    return edges;
}

fn isInside(
    edges: std.ArrayList(Line),
    p: P,
) bool {
    var inside = false;
    for (0..p.x) |x| {
        for (edges.items) |edge| {
            if (edge.isVertical() and isBetween(p.y, edge.p.y, edge.q.y) and edge.p.x == x)
                inside = !inside;
        }
    }
    return inside;
}

fn isRectInside(
    edges: std.ArrayList(Line),
    rect: Line,
) bool {
    const x = @min(rect.p.x, rect.q.x);
    const y = @min(rect.p.y, rect.q.y);

    var i: u32 = 0;
    var j: u32 = 0;
    while (i < width(rect)) : (i += 1) {
        while (j < height(rect)) : (j += 1) {
            const p = P{ .x = x + i, .y = y + j };
            if (!isInside(edges, p)) return false;
        }
    }

    return true;
}

pub fn part2(
    alloc: std.mem.Allocator,
    points: std.ArrayList(P),
) !void {
    var left: u32 = std.math.maxInt(u32);
    var right: u32 = 0;
    var bottom: u32 = std.math.maxInt(u32);
    var top: u32 = 0;
    for (points.items) |p| {
        if (p.x < left) left = p.x;
        if (p.x > right) right = p.x;
        if (p.y < bottom) bottom = p.y;
        if (p.y > top) top = p.y;
    }
    std.debug.print("Grid: ({d}, {d}) to ({d}, {d})\n", .{ left, bottom, right, top });

    var edges = try getEdges(alloc, points);
    defer edges.deinit(alloc);
    std.debug.print("Edge calculation done\n", .{});

    var rects = try getRects(alloc, points.items);
    defer rects.deinit(alloc);
    std.debug.print("Rect calculation done\n", .{});

    var max: u64 = 0;
    for (rects.items, 0..) |rect, i| {
        std.debug.print("  processed rect {d}/{d}\n", .{ i, rects.items.len });
        if (rect.area() > max and isRectInside(edges, rect)) {
            max = rect.area();
            std.debug.print(
                "  new max rect: ({d}, {d}) - ({d}, {d}),    area: {d}\n",
                .{ rect.p.x, rect.p.y, rect.q.x, rect.q.y, rect.area() },
            );
        }
    }
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

    try part1(gpa, points);
    try part2(gpa, points);
}
