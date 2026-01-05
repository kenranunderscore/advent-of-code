const std = @import("std");
const util = @import("util.zig");

const P = struct {
    x: u32,
    y: u32,

    fn init(x: u32, y: u32) P {
        return P{ .x = x, .y = y };
    }

    pub fn format(
        self: P,
        writer: anytype,
    ) !void {
        try writer.print("({d}, {d})", .{ self.x, self.y });
    }
};

const Line = struct {
    p: P,
    q: P,

    const Self = @This();

    fn init(p: P, q: P) Self {
        if (p.x < q.x or (p.x == q.x and p.y < q.y))
            return Self{ .p = p, .q = q }
        else
            return Self{ .p = q, .q = p };
    }

    fn area(self: Self) u64 {
        return (@abs(@as(i33, self.p.x) - @as(i33, self.q.x)) + 1) *
            (@abs(@as(i33, self.p.y) - @as(i33, self.q.y)) + 1);
    }

    fn isVertical(self: Self) bool {
        return self.p.x == self.q.x;
    }

    fn isHorizontal(self: Self) bool {
        return self.p.y == self.q.y;
    }

    pub fn format(
        self: Self,
        writer: anytype,
    ) !void {
        try writer.print("[{f} to {f}]", .{ self.p, self.q });
    }

    fn areaGreaterThan(_: void, orig: Line, other: Line) bool {
        return orig.area() > other.area();
    }
};

test "area" {
    try std.testing.expectEqual(5, Line.init(P.init(0, 0), P.init(0, 4)).area());
    try std.testing.expectEqual(12, Line.init(P.init(4, 0), P.init(1, 2)).area());
    try std.testing.expectEqual(12, Line.init(P.init(0, 0), P.init(2, 3)).area());
}

fn rectContainsPoint(rect: Line, p: P) bool {
    const min_x = @min(rect.p.x, rect.q.x);
    const min_y = @min(rect.p.y, rect.q.y);
    const max_x = @max(rect.p.x, rect.q.x);
    const max_y = @max(rect.p.y, rect.q.y);

    return min_x < p.x and p.x < max_x and min_y < p.y and p.y < max_y;
}

test "rectContainsPoint" {
    const rect = Line.init(P.init(0, 0), P.init(5, 7));
    try std.testing.expect(!rectContainsPoint(rect, P.init(0, 0)));
    try std.testing.expect(!rectContainsPoint(rect, P.init(5, 7)));
    try std.testing.expect(!rectContainsPoint(rect, P.init(4, 7)));
    try std.testing.expect(!rectContainsPoint(rect, P.init(2, 0)));
    try std.testing.expect(!rectContainsPoint(rect, P.init(0, 6)));

    var i: u32 = 1;
    while (i < 5) : (i += 1) {
        var j: u32 = 1;
        while (j < 7) : (j += 1) {
            try std.testing.expect(rectContainsPoint(rect, P.init(i, j)));
        }
    }
}

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
    points: []const P,
) !std.ArrayList(Line) {
    const n_rects = @divExact(points.len * (points.len - 1), 2);

    var rects: std.ArrayList(Line) = try .initCapacity(alloc, n_rects);

    for (0..points.len - 1) |i| {
        for (i + 1..points.len) |j| {
            try rects.append(alloc, Line.init(points[i], points[j]));
        }
    }

    return rects;
}

pub fn part1(
    alloc: std.mem.Allocator,
    points: []const P,
) !void {
    var rects = try getRects(alloc, points);
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

fn containsPoint(points: []const P, p: P) bool {
    for (points) |q| {
        if (q.x == p.x and q.y == p.y) return true;
    }

    return false;
}

fn getEdges(
    alloc: std.mem.Allocator,
    points: []const P,
) !std.ArrayList(Line) {
    var edges: std.ArrayList(Line) = .empty;

    for (0..points.len - 1) |i| {
        const edge = Line.init(points[i], points[i + 1]);
        try edges.append(alloc, edge);
    }

    // Connect last and first point
    try edges.append(alloc, Line.init(points[points.len - 1], points[0]));

    return edges;
}

fn isInside(
    edges: []const Line,
    p: P,
) bool {
    var inside = false;
    outer: for (edges) |edge| {
        if (edge.isVertical() and edge.p.x == p.x and isBetween(p.y, edge.p.y, edge.q.y)) {
            return true;
        }

        if (edge.isHorizontal() and edge.p.y == p.y and isBetween(p.x, edge.p.x, edge.q.x)) {
            return true;
        }

        for (0..p.x) |x| {
            if (edge.isVertical() and @min(edge.p.y, edge.q.y) < p.y and p.y < @max(edge.p.y, edge.q.y) and edge.p.x == x)
                inside = !inside;

            // Use that edges are left-to-right and we cast the ray right-to-left
            if (edge.isHorizontal() and p.y == edge.p.y and edge.q.x == x) {
                inside = !inside;
                continue :outer;
            }
        }
    }

    return inside;
}

const example_edges: []const Line = &[_]Line{
    Line.init(P.init(7, 1), P.init(11, 1)),
    Line.init(P.init(11, 1), P.init(11, 7)),
    Line.init(P.init(11, 7), P.init(9, 7)),
    Line.init(P.init(9, 7), P.init(9, 5)),
    Line.init(P.init(9, 5), P.init(2, 5)),
    Line.init(P.init(2, 5), P.init(2, 3)),
    Line.init(P.init(2, 3), P.init(7, 3)),
    Line.init(P.init(7, 3), P.init(7, 1)),
};

test "isInside" {
    // Inside the true interior
    try std.testing.expect(isInside(example_edges, P.init(9, 2)));
    try std.testing.expect(isInside(example_edges, P.init(9, 4)));
    try std.testing.expect(isInside(example_edges, P.init(5, 4)));
    try std.testing.expect(isInside(example_edges, P.init(3, 4)));
    try std.testing.expect(isInside(example_edges, P.init(10, 6)));
    try std.testing.expect(isInside(example_edges, P.init(8, 3)));

    // Points between edge ends
    try std.testing.expect(isInside(example_edges, P.init(7, 2)));
    try std.testing.expect(isInside(example_edges, P.init(11, 6)));
    try std.testing.expect(isInside(example_edges, P.init(6, 5)));

    // Edge ends
    for (example_edges) |edge| {
        try std.testing.expect(isInside(example_edges, edge.p));
    }
}

fn isRectInside(
    edges: []const Line,
    rect: Line,
) bool {
    if (!isInside(edges, P.init(rect.p.x, rect.q.y))) return false;
    if (!isInside(edges, P.init(rect.q.x, rect.p.y))) return false;

    var x = rect.p.x;
    while (x <= rect.q.x) : (x += 1) {
        if (!isInside(edges, P.init(x, rect.p.y))) return false;
        if (!isInside(edges, P.init(x, rect.q.y))) return false;
    }

    var y = @min(rect.p.y, rect.q.y);
    while (y <= @max(rect.p.y, rect.q.y)) : (y += 1) {
        if (!isInside(edges, P.init(rect.p.x, y))) return false;
        if (!isInside(edges, P.init(rect.q.x, y))) return false;
    }

    return true;
}

test "isRectInside" {
    try std.testing.expect(isRectInside(example_edges, Line.init(P.init(7, 3), P.init(11, 1))));
    try std.testing.expect(isRectInside(example_edges, Line.init(P.init(9, 7), P.init(9, 5))));
    try std.testing.expect(isRectInside(example_edges, Line.init(P.init(9, 5), P.init(2, 3))));
    try std.testing.expect(!isRectInside(example_edges, Line.init(P.init(7, 1), P.init(9, 7))));
    try std.testing.expect(!isRectInside(example_edges, Line.init(P.init(7, 1), P.init(11, 7))));
}

test "isRectInside counter-example" {
    const example: []const Line = &[_]Line{
        Line.init(P.init(0, 2), P.init(0, 6)),
        Line.init(P.init(0, 6), P.init(7, 6)),
        Line.init(P.init(7, 6), P.init(7, 2)),
        Line.init(P.init(7, 2), P.init(5, 2)),
        Line.init(P.init(5, 2), P.init(5, 4)),
        Line.init(P.init(5, 4), P.init(4, 4)),
        Line.init(P.init(4, 4), P.init(4, 0)),
        Line.init(P.init(4, 0), P.init(3, 0)),
        Line.init(P.init(3, 0), P.init(3, 4)),
        Line.init(P.init(3, 4), P.init(2, 4)),
        Line.init(P.init(2, 4), P.init(2, 2)),
        Line.init(P.init(2, 2), P.init(0, 2)),
    };
    try std.testing.expect(isRectInside(example, Line.init(P.init(0, 2), P.init(7, 2))));
    try std.testing.expect(isRectInside(example, Line.init(P.init(2, 2), P.init(4, 4))));
    try std.testing.expect(!isRectInside(example, Line.init(P.init(4, 0), P.init(7, 2))));
}

pub fn part2(
    alloc: std.mem.Allocator,
    points: []const P,
) !void {
    var left: u32 = std.math.maxInt(u32);
    var right: u32 = 0;
    var bottom: u32 = std.math.maxInt(u32);
    var top: u32 = 0;
    for (points) |p| {
        if (p.x < left) left = p.x;
        if (p.x > right) right = p.x;
        if (p.y < bottom) bottom = p.y;
        if (p.y > top) top = p.y;
    }
    std.debug.print("Grid: ({d}, {d}) to ({d}, {d})\n", .{ left, bottom, right, top });

    var edges = try getEdges(alloc, points);
    defer edges.deinit(alloc);
    std.debug.print("Edge calculation done\n", .{});

    var rects = try getRects(alloc, points);
    defer rects.deinit(alloc);
    std.sort.heap(Line, rects.items, {}, Line.areaGreaterThan);
    std.debug.print("Rect calculation done\n", .{});

    for (rects.items, 0..) |rect, i| {
        std.debug.print("  processing rect {d}/{d}: {f}\n", .{ i, rects.items.len, rect });
        if (isRectInside(edges.items, rect)) {
            std.debug.print("    rect is inside! area: {d}\n", .{rect.area()});
            break;
        } else {
            std.debug.print("    X\n", .{});
        }
    }
}

pub fn main() !void {
    var alloc = std.heap.GeneralPurposeAllocator(.{}).init;
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

    try part1(gpa, points.items);
    try part2(gpa, points.items);
}
