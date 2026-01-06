const std = @import("std");
const util = @import("util.zig");

const print = std.debug.print;

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

/// Axis-aligned is assumed
const Line = struct {
    p: P,
    q: P,

    fn init(p: P, q: P) Line {
        if (p.x < q.x or (p.x == q.x and p.y < q.y))
            return Line{ .p = p, .q = q }
        else
            return Line{ .p = q, .q = p };
    }

    fn isVertical(self: Line) bool {
        return self.p.x == self.q.x;
    }

    fn isHorizontal(self: Line) bool {
        return self.p.y == self.q.y;
    }

    pub fn format(
        self: Line,
        writer: anytype,
    ) !void {
        try writer.print("({f}, {f})", .{ self.p, self.q });
    }

    /// Assumes different orientation, and non-zero length
    fn intersects(self: Line, other: Line) bool {
        if (self.isHorizontal()) {
            return other.isVertical() and
                self.p.x < other.p.x and
                other.p.x < self.q.x and
                @min(other.p.y, other.q.y) < self.p.y and
                @max(other.p.y, other.q.y) > self.p.y;
        } else {
            return other.isHorizontal() and
                @min(self.p.y, self.q.y) < other.p.y and
                @max(self.p.y, self.q.y) > other.p.y and
                other.p.x < self.p.x and
                other.q.x > self.p.x;
        }
    }
};

const Loop = struct {
    edges: std.ArrayList(Line),
    alloc: std.mem.Allocator,

    fn init(
        alloc: std.mem.Allocator,
        points: []const P,
    ) !Loop {
        var edges: std.ArrayList(Line) = .empty;

        for (0..points.len - 1) |i| {
            const edge = Line.init(points[i], points[i + 1]);
            try edges.append(alloc, edge);
        }

        // Connect last and first point
        try edges.append(alloc, Line.init(points[points.len - 1], points[0]));

        return Loop{ .alloc = alloc, .edges = edges };
    }

    fn deinit(self: *Loop) void {
        self.edges.deinit(self.alloc);
    }

    fn contains(
        self: Loop,
        p: P,
    ) bool {
        var inside = false;
        for (self.edges.items) |edge| {
            if (edge.isVertical() and edge.p.x == p.x and isBetween(p.y, edge.p.y, edge.q.y)) {
                return true;
            }

            if (edge.isHorizontal() and edge.p.y == p.y and isBetween(p.x, edge.p.x, edge.q.x)) {
                return true;
            }

            if (edge.isVertical() and
                edge.p.x < p.x and
                @min(edge.p.y, edge.q.y) < p.y and
                p.y <= @max(edge.p.y, edge.q.y))
            {
                inside = !inside;
            }
        }

        return inside;
    }

    fn containsRect(
        self: Loop,
        rect: Rect,
    ) bool {
        // Skip rectangles whose corners aren't even on the inside
        if (!self.contains(P.init(rect.p.x, rect.q.y))) return false;
        if (!self.contains(P.init(rect.q.x, rect.p.y))) return false;

        for (self.edges.items) |edge| {
            if (rect.trulyContains(edge.p) or rect.trulyContains(edge.q)) {
                // `edge` intersects the rectangle: Now if any of the points diagonally adjacent
                // to the end point inside rect is "outside", that is cut out of the rectangle,
                // making it invalid.
                const z = if (rect.trulyContains(edge.p)) edge.p else edge.q;
                if (z.x > 0 and z.y > 0 and !self.contains(P.init(z.x - 1, z.y - 1)))
                    return false;
                if (z.x > 0 and !self.contains(P.init(z.x - 1, z.y + 1)))
                    return false;
                if (z.y > 0 and !self.contains(P.init(z.x + 1, z.y - 1)))
                    return false;
                if (!self.contains(P.init(z.x + 1, z.y + 1)))
                    return false;
            } else {
                // If `edge` still intersects `rect`, it goes through the whole thing: Now find
                // a point truly inside rect and check its two neighbors. This rules out some
                // edge cases where adjacent and parallel lines go through the rectangle, but
                // not carving out anything (due to lines having non-zero width in integer
                // scenarios such as this one).
                const px_qy = P.init(rect.p.x, rect.q.y);
                const qx_py = P.init(rect.q.x, rect.p.y);
                const h1 = Line.init(rect.p, qx_py);
                const h2 = Line.init(px_qy, rect.q);
                const v1 = Line.init(rect.p, px_qy);
                const v2 = Line.init(rect.q, qx_py);

                if (edge.intersects(h1) or edge.intersects(h2) or edge.intersects(v1) or edge.intersects(v2)) {
                    // intersects checks for "true" intersection, and the rect isn't degenerate,
                    // so we can just create points on the line and inside the rect
                    if (edge.isHorizontal()) {
                        const z = P.init(rect.p.x + 1, edge.p.y);
                        if (z.y > 0 and !self.contains(P.init(z.x, z.y - 1)) or !self.contains(P.init(z.x, z.y + 1)))
                            return false;
                    } else {
                        const z = P.init(edge.p.x, @min(rect.p.y, rect.q.y) + 1);
                        if (z.x > 0 and !self.contains(P.init(z.x - 1, z.y)) or !self.contains(P.init(z.x + 1, z.y)))
                            return false;
                    }
                }
            }
        }

        return true;
    }
};

const Rect = struct {
    p: P,
    q: P,

    fn init(p: P, q: P) Rect {
        if (p.x < q.x or (p.x == q.x and p.y < q.y))
            return Rect{ .p = p, .q = q }
        else
            return Rect{ .p = q, .q = p };
    }

    fn area(self: Rect) u64 {
        return (@abs(@as(i33, self.p.x) - @as(i33, self.q.x)) + 1) *
            (@abs(@as(i33, self.p.y) - @as(i33, self.q.y)) + 1);
    }

    fn areaGreaterThan(_: void, orig: Rect, other: Rect) bool {
        return orig.area() > other.area();
    }

    pub fn format(
        self: Rect,
        writer: anytype,
    ) !void {
        try writer.print("[{f} to {f}]", .{ self.p, self.q });
    }

    fn trulyContains(self: Rect, p: P) bool {
        const min_x = @min(self.p.x, self.q.x);
        const min_y = @min(self.p.y, self.q.y);
        const max_x = @max(self.p.x, self.q.x);
        const max_y = @max(self.p.y, self.q.y);

        return min_x < p.x and p.x < max_x and min_y < p.y and p.y < max_y;
    }
};

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
) !std.ArrayList(Rect) {
    const n_rects = @divExact(points.len * (points.len - 1), 2);

    var rects: std.ArrayList(Rect) = try .initCapacity(alloc, n_rects);

    for (0..points.len - 1) |i| {
        for (i + 1..points.len) |j| {
            try rects.append(alloc, Rect.init(points[i], points[j]));
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
    for (rects.items) |rect| {
        if (rect.area() > max) {
            max = rect.area();
        }
    }

    print("Part 1: {d}\n", .{max});
}

fn containsPoint(points: []const P, p: P) bool {
    for (points) |q| {
        if (q.x == p.x and q.y == p.y) return true;
    }

    return false;
}

const example_points: []const P = &[_]P{
    P.init(7, 1),
    P.init(11, 1),
    P.init(11, 7),
    P.init(9, 7),
    P.init(9, 5),
    P.init(2, 5),
    P.init(2, 3),
    P.init(7, 3),
};

pub fn part2(
    alloc: std.mem.Allocator,
    points: []const P,
) !void {
    var loop = try Loop.init(alloc, points);
    defer loop.deinit();
    print("Loop calculation done\n", .{});

    var rects = try getRects(alloc, points);
    defer rects.deinit(alloc);
    std.sort.heap(Rect, rects.items, {}, Rect.areaGreaterThan);
    print("Rect calculation done\n", .{});

    for (rects.items, 0..) |rect, i| {
        print("  Processing rect {d}/{d}\n", .{ i, rects.items.len });
        if (loop.containsRect(rect)) {
            print("    Found it! Its area is {d}\n", .{rect.area()});
            break;
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

test "Rect.area" {
    try std.testing.expectEqual(5, Rect.init(P.init(0, 0), P.init(0, 4)).area());
    try std.testing.expectEqual(12, Rect.init(P.init(4, 0), P.init(1, 2)).area());
    try std.testing.expectEqual(12, Rect.init(P.init(0, 0), P.init(2, 3)).area());
}

test "Rect.containsPoint" {
    const rect = Rect.init(P.init(0, 0), P.init(5, 7));
    try std.testing.expect(!rect.trulyContains(P.init(0, 0)));
    try std.testing.expect(!rect.trulyContains(P.init(5, 7)));
    try std.testing.expect(!rect.trulyContains(P.init(4, 7)));
    try std.testing.expect(!rect.trulyContains(P.init(2, 0)));
    try std.testing.expect(!rect.trulyContains(P.init(0, 6)));

    var i: u32 = 1;
    while (i < 5) : (i += 1) {
        var j: u32 = 1;
        while (j < 7) : (j += 1) {
            try std.testing.expect(rect.trulyContains(P.init(i, j)));
        }
    }
}

test "Loop.containsPoint" {
    var loop = try Loop.init(std.testing.allocator, example_points);
    defer loop.deinit();

    // Inside the true interior
    try std.testing.expect(loop.contains(P.init(9, 2)));
    try std.testing.expect(loop.contains(P.init(9, 4)));
    try std.testing.expect(loop.contains(P.init(5, 4)));
    try std.testing.expect(loop.contains(P.init(3, 4)));
    try std.testing.expect(loop.contains(P.init(10, 6)));
    try std.testing.expect(loop.contains(P.init(8, 3)));

    // Points between edge ends
    try std.testing.expect(loop.contains(P.init(7, 2)));
    try std.testing.expect(loop.contains(P.init(11, 6)));
    try std.testing.expect(loop.contains(P.init(6, 5)));

    // Edge ends
    for (example_points) |p| {
        try std.testing.expect(loop.contains(p));
    }

    // Outside points
    try std.testing.expect(!loop.contains(P.init(12, 1)));
}

test "Loop.containsRect" {
    var loop = try Loop.init(std.testing.allocator, example_points);
    defer loop.deinit();

    try std.testing.expect(loop.containsRect(Rect.init(P.init(7, 3), P.init(11, 1))));
    try std.testing.expect(loop.containsRect(Rect.init(P.init(9, 7), P.init(9, 5))));
    try std.testing.expect(loop.containsRect(Rect.init(P.init(9, 5), P.init(2, 3))));
    try std.testing.expect(!loop.containsRect(Rect.init(P.init(7, 1), P.init(9, 7))));
    try std.testing.expect(!loop.containsRect(Rect.init(P.init(7, 1), P.init(11, 7))));
}

test "isRectInside counter-example" {
    const points = &[_]P{
        P.init(0, 2),
        P.init(0, 6),
        P.init(7, 6),
        P.init(7, 2),
        P.init(5, 2),
        P.init(5, 4),
        P.init(4, 4),
        P.init(4, 0),
        P.init(3, 0),
        P.init(3, 4),
        P.init(2, 4),
        P.init(2, 2),
    };
    var loop = try Loop.init(std.testing.allocator, points);
    defer loop.deinit();

    try std.testing.expect(!loop.contains(P.init(7, 0)));
    try std.testing.expect(loop.containsRect(Rect.init(P.init(0, 2), P.init(7, 2))));
    try std.testing.expect(loop.containsRect(Rect.init(P.init(2, 2), P.init(4, 4))));
    try std.testing.expect(!loop.containsRect(Rect.init(P.init(4, 0), P.init(7, 2))));
}
