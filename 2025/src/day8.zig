const std = @import("std");
const util = @import("util.zig");

const Point = struct {
    x: u64,
    y: u64,
    z: u64,

    fn distSquared(p: Point, q: Point) u64 {
        const result = (std.math.powi(i65, @as(i65, p.x) - @as(i65, q.x), 2) catch unreachable) +
            (std.math.powi(i65, @as(i65, p.y) - @as(i65, q.y), 2) catch unreachable) +
            (std.math.powi(i65, @as(i65, p.z) - @as(i65, q.z), 2) catch unreachable);
        return @intCast(result);
    }

    pub fn format(
        self: Point,
        writer: anytype,
    ) !void {
        try writer.print("({}, {}, {})", .{ self.x, self.y, self.z });
    }
};

const Context = struct {
    points: std.ArrayList(Point),
    alloc: std.mem.Allocator,

    const Self = @This();

    fn init(alloc: std.mem.Allocator) Self {
        return Self{
            .points = .empty,
            .alloc = alloc,
        };
    }

    fn deinit(self: *Self) void {
        self.points.deinit(self.alloc);
    }
};

fn parsePoint(line: []const u8) !Point {
    var tokens = std.mem.tokenizeScalar(u8, line, ',');
    return Point{
        .x = try std.fmt.parseInt(u64, tokens.next().?, 10),
        .y = try std.fmt.parseInt(u64, tokens.next().?, 10),
        .z = try std.fmt.parseInt(u64, tokens.next().?, 10),
    };
}

fn callback(line: []const u8, ctx: *Context) !void {
    if (line.len == 0) return;
    try ctx.points.append(ctx.alloc, try parsePoint(line));
}

const Pair = struct {
    p: Point,
    q: Point,

    fn dist(self: Pair) u64 {
        return self.p.distSquared(self.q);
    }
};

fn distLessThan(_: void, orig: Pair, other: Pair) bool {
    return orig.dist() < other.dist();
}

fn getOrderedPairs(
    alloc: std.mem.Allocator,
    points: []Point,
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

    std.sort.heap(Pair, pairs.items, {}, distLessThan);
    return pairs;
}

const Circuit = struct {
    points: std.AutoHashMap(Point, void),
    label: u16,

    const Self = @This();

    fn init(alloc: std.mem.Allocator, label: u16) Self {
        const points = std.AutoHashMap(Point, void).init(alloc);
        return Self{ .points = points, .label = label };
    }

    fn deinit(self: *Self) void {
        self.points.deinit();
    }

    fn contains(self: Self, p: Point) bool {
        return self.points.contains(p);
    }

    fn put(self: *Self, p: Point) !void {
        try self.points.put(p, {});
    }

    fn lessThan(_: void, c1: Self, c2: Self) bool {
        return c1.points.count() < c2.points.count();
    }

    fn subsume(self: *Self, other: *Self) !void {
        var iter = other.points.iterator();
        while (iter.next()) |p| {
            try self.put(p.key_ptr.*);
        }
        other.points.clearAndFree();
    }
};

const Results = struct { part1: usize, part2: usize };

fn run(ctx: Context) !Results {
    var pairs = try getOrderedPairs(ctx.alloc, ctx.points.items);
    defer pairs.deinit(ctx.alloc);

    const n: usize = 1000;

    var circuits: std.ArrayList(Circuit) = try .initCapacity(ctx.alloc, n);
    defer {
        for (circuits.items) |*c| c.deinit();
        circuits.deinit(ctx.alloc);
    }

    for (ctx.points.items, 0..) |p, i| {
        var circuit = Circuit.init(ctx.alloc, @intCast(i));
        try circuit.put(p);
        try circuits.append(ctx.alloc, circuit);
    }

    var i: usize = 0;
    var part1: usize = undefined;
    outer: while (i < pairs.items.len) : (i += 1) {
        if (i == 999) {
            const l = circuits.items.len;
            std.sort.heap(Circuit, circuits.items, {}, Circuit.lessThan);
            part1 = circuits.items[l - 1].points.count() *
                circuits.items[l - 2].points.count() *
                circuits.items[l - 3].points.count();
        }

        const pair = pairs.items[i];

        var first: ?*Circuit = null;
        for (0..circuits.items.len) |j| {
            var circuit = &circuits.items[j];
            // TODO: benchmark checking just once
            if (circuit.contains(pair.p) and circuit.contains(pair.q)) {
                continue :outer;
            }

            if (circuit.contains(pair.p) or circuit.contains(pair.q)) {
                if (first == null) {
                    first = circuit;
                    continue;
                }

                try first.?.subsume(circuit);
                _ = circuits.orderedRemove(j);

                if (circuits.items.len == 1) {
                    return Results{
                        .part1 = part1,
                        .part2 = pair.p.x * pair.q.x,
                    };
                }

                continue :outer;
            }
        }
    }

    unreachable;
}

test "part 1" {
    const a = std.testing.allocator;
    var ctx: Context = .init(a);
    defer ctx.deinit();

    try util.processFile(a, "input/day8", &ctx, callback, '\n');

    const res = try run(ctx);
    try std.testing.expectEqual(129_564, res.part1);
    try std.testing.expectEqual(42_047_840, res.part2);
}
