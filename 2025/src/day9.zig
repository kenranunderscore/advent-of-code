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
        try writer.print("{{f}, {f}}", .{ self.p, self.q });
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

        // Walk the four edges of the rectangle
        var x = rect.p.x;
        while (x <= rect.q.x) : (x += 1) {
            if (!self.contains(P.init(x, rect.p.y))) return false;
            if (!self.contains(P.init(x, rect.q.y))) return false;
        }

        var y = @min(rect.p.y, rect.q.y);
        while (y <= @max(rect.p.y, rect.q.y)) : (y += 1) {
            if (!self.contains(P.init(rect.p.x, y))) return false;
            if (!self.contains(P.init(rect.q.x, y))) return false;
        }

        // Observation (either wrong or badly implemented):
        //   - For any rectangle whose corners are on the inside but that they aren't fully inside,
        //     there needs to exist an outer edge intersecting it. The problem that many online
        //     solutions seem to overlook (as it doesn't exist in the input) is that there are edge
        //     cases where the rectangle has (2 or more) real intersections (possibly even with both
        //     of their end points outside of the rectangle) but nevertheless is fully on the inside.
        //     This is due to the integer nature of the grid (edges have a width, and thus two
        //     neighboring edges don't leave any gap in the rectangle). That's hard to test for...
        //   - One idea to check for this is that in in any of these edge cases, a 90-degree angle
        //     vertex needs to lie inside the rectangle. So if that's the case, we could check the
        //     diagonally adjacent fields of this vertex: if this edge does indeed cut something out
        //     of the rectangle, one of these four fields should be "it", if I'm not mistaken. The
        //     code below fails, though, but sadly not in any of my test cases...
        //
        // for (edges) |edge| {
        //     if (rectContainsPoint(rect, edge.p)) {
        //         std.debug.print("  rect {f} contains p {f}\n", .{ rect, edge.p });
        //         if (edge.p.x > 0 and edge.p.y > 0 and !isInside(edges, P.init(edge.p.x - 1, edge.p.y - 1))) {
        //             std.debug.print("      {f} outside 1\n", .{edge.p});
        //             return false;
        //         }
        //         if (edge.p.x > 0 and !isInside(edges, P.init(edge.p.x - 1, edge.p.y + 1))) {
        //             std.debug.print("      {f} outside 2\n", .{edge.p});
        //             return false;
        //         }
        //         if (edge.p.y > 0 and !isInside(edges, P.init(edge.p.x + 1, edge.p.y - 1))) {
        //             std.debug.print("      {f} outside 3\n", .{edge.p});
        //             return false;
        //         }
        //         if (!isInside(edges, P.init(edge.p.x + 1, edge.p.y + 1))) {
        //             std.debug.print("      {f} outside 4\n", .{edge.p});
        //             return false;
        //         }
        //     }
        //     if (rectContainsPoint(rect, edge.q)) {
        //         std.debug.print("  rect {f} contains q {f}\n", .{ rect, edge.p });
        //         if (edge.q.x > 0 and edge.q.y > 0 and !isInside(edges, P.init(edge.q.x - 1, edge.q.y - 1))) {
        //             std.debug.print("      {f} outside 1\n", .{edge.p});
        //             return false;
        //         }
        //         if (edge.q.x > 0 and !isInside(edges, P.init(edge.q.x - 1, edge.q.y + 1))) {
        //             std.debug.print("      {f} outside 2\n", .{edge.p});
        //             return false;
        //         }
        //         if (edge.q.y > 0 and !isInside(edges, P.init(edge.q.x + 1, edge.q.y - 1))) {
        //             std.debug.print("      {f} outside 3\n", .{edge.p});
        //             return false;
        //         }
        //         if (!isInside(edges, P.init(edge.q.x + 1, edge.q.y + 1))) {
        //             std.debug.print("      {f} outside 4\n", .{edge.p});
        //             return false;
        //         }
        //     }
        // }

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

    std.debug.print("Part 1: {d}\n", .{max});
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

    var loop = try Loop.init(alloc, points);
    defer loop.deinit();
    std.debug.print("Loop calculation done\n", .{});

    var rects = try getRects(alloc, points);
    defer rects.deinit(alloc);
    std.sort.heap(Rect, rects.items, {}, Rect.areaGreaterThan);
    std.debug.print("Rect calculation done\n", .{});

    for (rects.items, 0..) |rect, i| {
        std.debug.print("  processing rect {d}/{d}: {f}\n", .{ i, rects.items.len, rect });
        if (loop.containsRect(rect)) {
            std.debug.print("    rect is inside! area: {d}\n", .{rect.area()});
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
