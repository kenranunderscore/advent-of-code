const std = @import("std");
const util = @import("util.zig");

const MAX_LEN = 10;

const Joltage = [MAX_LEN]u16;

const Machine = struct {
    target_state: u10,
    buttons: std.ArrayList(u10),
    target_joltage: Joltage,
};

const Context = struct {
    machines: std.ArrayList(Machine),
    alloc: std.mem.Allocator,

    const Self = @This();

    fn init(alloc: std.mem.Allocator) Self {
        return Self{ .alloc = alloc, .machines = .empty };
    }

    fn deinit(self: *Self) void {
        for (self.machines.items) |*machine| {
            machine.buttons.deinit(self.alloc);
        }
        self.machines.deinit(self.alloc);
    }

    fn addMachine(self: *Self, machine: Machine) !void {
        try self.machines.append(self.alloc, machine);
    }
};

fn parseState(input: []const u8) u10 {
    var state: u10 = 0;
    var i: u4 = 1;
    while (i < input.len - 1) : (i += 1) {
        if (input[i] == '#')
            state |= @as(u10, 1) << (MAX_LEN - i);
    }
    return state;
}

test "parseState" {
    try std.testing.expectEqual(0, parseState("[.]"));
    try std.testing.expectEqual(0, parseState("[..........]"));
    try std.testing.expectEqual(0b1000000000, parseState("[#]"));
    try std.testing.expectEqual(0b1100000000, parseState("[##]"));
    try std.testing.expectEqual(0b0000000011, parseState("[........##]"));
    try std.testing.expectEqual(0b1111111111, parseState("[##########]"));
    try std.testing.expectEqual(0b0101001000, parseState("[.#.#..#]"));
}

fn parseButton(input: []const u8) !u10 {
    if (input[0] != '(') return error.NotAButton;

    const without_parens = input[1 .. input.len - 1];
    if (without_parens.len == 0) return 0;

    var iter = std.mem.splitScalar(u8, without_parens, ',');
    var button: u10 = 0;

    while (iter.next()) |char| {
        const n = try std.fmt.parseInt(u4, char, 10);
        button |= @as(u10, 1) << (MAX_LEN - n - 1);
    }

    return button;
}

test "parseButton" {
    try std.testing.expectEqual(0, try parseButton("()"));
    try std.testing.expectEqual(0b1000000000, try parseButton("(0)"));
    try std.testing.expectEqual(0b0100010100, try parseButton("(1,5,7)"));
    try std.testing.expectEqual(0b0000000001, try parseButton("(9)"));
    try std.testing.expectEqual(0b1111111111, try parseButton("(0,1,2,3,4,5,6,7,8,9)"));
    try std.testing.expectError(error.NotAButton, parseButton("{8,3}"));
}

fn parseJoltage(input: []const u8) !Joltage {
    var iter = std.mem.splitScalar(u8, input[1 .. input.len - 1], ',');
    var joltage = [_]u16{0} ** MAX_LEN;

    var i: usize = 0;
    while (iter.next()) |s| {
        const n = try std.fmt.parseInt(u16, s, 10);
        joltage[i] = n;
        i += 1;
    }

    return joltage;
}

test "parseJoltage" {
    var res = try parseJoltage("{1,3}");
    try std.testing.expectEqualSlices(u16, &[_]u16{ 1, 3, 0, 0, 0, 0, 0, 0, 0, 0 }, &res);

    res = try parseJoltage("{17,5,0,0,0,0,0,13}");
    try std.testing.expectEqualSlices(u16, &[_]u16{ 17, 5, 0, 0, 0, 0, 0, 13, 0, 0 }, &res);
}

fn pressButton(state: u10, button: u10) u10 {
    return state ^ button;
}

test "pressButton" {
    try std.testing.expectEqual(0b1000100011, pressButton(0, 0b1000100011));
    try std.testing.expectEqual(0, pressButton(0b101, 0b101));
}

fn adjustJoltage(joltage: *Joltage, button: u10) void {
    var i: u4 = 0;
    while (i < MAX_LEN) : (i += 1) {
        const bit = @as(u10, 1) << (MAX_LEN - i - 1);
        if (button & bit > 0)
            joltage[i] += 1;
    }
}

test "adjustJoltage" {
    var joltage = [_]u16{0} ** MAX_LEN;

    adjustJoltage(&joltage, 0b1011100000);
    try std.testing.expectEqualSlices(u16, &[_]u16{ 1, 0, 1, 1, 1, 0, 0, 0, 0, 0 }, &joltage);

    adjustJoltage(&joltage, 0);
    try std.testing.expectEqualSlices(u16, &[_]u16{ 1, 0, 1, 1, 1, 0, 0, 0, 0, 0 }, &joltage);

    adjustJoltage(&joltage, 0b1110000111);
    try std.testing.expectEqualSlices(u16, &[_]u16{ 2, 1, 2, 1, 1, 0, 0, 1, 1, 1 }, &joltage);

    adjustJoltage(&joltage, 0b1000100000);
    try std.testing.expectEqualSlices(u16, &[_]u16{ 3, 1, 2, 1, 2, 0, 0, 1, 1, 1 }, &joltage);
}

fn parseMachine(alloc: std.mem.Allocator, line: []const u8) !Machine {
    var iter = std.mem.splitScalar(u8, line, ' ');
    const state = parseState(iter.next().?);

    var buttons: std.ArrayList(u10) = .empty;
    var joltage: Joltage = undefined;
    while (iter.next()) |s| {
        const btn = parseButton(s) catch {
            joltage = try parseJoltage(s);
            break;
        };
        try buttons.append(alloc, btn);
    }

    return Machine{
        .target_state = state,
        .buttons = buttons,
        .target_joltage = joltage,
    };
}

fn callback(line: []const u8, ctx: *Context) !void {
    const machine = try parseMachine(ctx.alloc, line);
    try ctx.addMachine(machine);
}

fn combinationsHelper(
    comptime T: type,
    alloc: std.mem.Allocator,
    items: []const T,
    n: usize,
    start: usize,
    buf: *std.ArrayList(T),
    ctx: anytype,
    comptime CallbackFn: fn (@TypeOf(ctx), []const T) bool,
    reuse: bool,
) !bool {
    if (buf.items.len == n) {
        return CallbackFn(ctx, buf.items);
    }

    const end = if (reuse) items.len else items.len - (n - buf.items.len) + 1;
    var i = start;
    while (i < end) : (i += 1) {
        try buf.append(alloc, items[i]);
        defer _ = buf.pop();

        const next = if (reuse) i else i + 1;
        if (try combinationsHelper(T, alloc, items, n, next, buf, ctx, CallbackFn, reuse))
            return true;
    }

    return false;
}

fn combinationsWithReuse(
    comptime T: type,
    alloc: std.mem.Allocator,
    items: []const T,
    n: usize,
    buf: *std.ArrayList(T),
    ctx: anytype,
    comptime CallbackFn: fn (@TypeOf(ctx), []const T) bool,
) !bool {
    return combinationsHelper(T, alloc, items, n, 0, buf, ctx, CallbackFn, true);
}

fn combinations(
    comptime T: type,
    alloc: std.mem.Allocator,
    items: []const T,
    n: usize,
    buf: *std.ArrayList(T),
    ctx: anytype,
    comptime CallbackFn: fn (@TypeOf(ctx), []const T) bool,
) !bool {
    return combinationsHelper(T, alloc, items, n, 0, buf, ctx, CallbackFn, false);
}

fn part1(alloc: std.mem.Allocator, ctx: Context) !usize {
    const Iter = struct {
        button_presses: usize = 0,
        current_machine: ?Machine = null,

        pub fn visit(self: *@This(), buttons: []const u10) bool {
            var state: u10 = 0;
            for (buttons) |btn|
                state = pressButton(state, btn);

            if (self.current_machine.?.target_state == state) {
                self.button_presses += buttons.len;
                return true;
            }

            return false;
        }
    };

    var iter = Iter{};
    var buf: std.ArrayList(u10) = .empty;
    defer buf.deinit(alloc);

    outer: for (ctx.machines.items) |machine| {
        iter.current_machine = machine;
        var i: u8 = 0;
        while (true) : (i += 1) {
            const found = try combinations(u10, alloc, machine.buttons.items, i, &buf, &iter, Iter.visit);
            buf.clearAndFree(alloc);
            if (found) continue :outer;
        }
    }

    return iter.button_presses;
}

fn part2(alloc: std.mem.Allocator, ctx: Context) !usize {
    const Iter = struct {
        button_presses: usize = 0,
        current_machine: ?Machine = null,

        pub fn visit(self: *@This(), buttons: []const u10) bool {
            var joltage = [_]u16{0} ** MAX_LEN;
            for (buttons) |btn|
                adjustJoltage(&joltage, btn);

            if (std.mem.eql(u16, &self.current_machine.?.target_joltage, &joltage)) {
                std.debug.print("  found! took {d} tries\n", .{buttons.len});
                self.button_presses += buttons.len;
                return true;
            }

            return false;
        }
    };

    var iter = Iter{};
    var buf: std.ArrayList(u10) = .empty;
    defer buf.deinit(alloc);

    outer: for (ctx.machines.items, 0..) |machine, n| {
        std.debug.print("processing machine {d}...\n", .{n});
        iter.current_machine = machine;
        var i: u8 = 0;
        while (true) : (i += 1) {
            std.debug.print("  depth {d}\n", .{i});
            const found = try combinationsWithReuse(u10, alloc, machine.buttons.items, i, &buf, &iter, Iter.visit);
            buf.clearAndFree(alloc);
            if (found) continue :outer;
        }
    }

    return iter.button_presses;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer std.debug.assert(gpa.deinit() == .ok);
    const alloc = gpa.allocator();

    var ctx = Context.init(alloc);
    defer ctx.deinit();

    try util.processFile(alloc, "input/day10", &ctx, callback, '\n');

    const part1_result = try part1(alloc, ctx);
    const part2_result = try part2(alloc, ctx);

    std.debug.print("Part 1: {d}\nPart 2: {d}\n", .{ part1_result, part2_result });
}

test "combinations" {
    const alloc = std.testing.allocator;

    const items = [_]u16{ 1, 2, 3, 4 };
    var buf: std.ArrayList(u16) = .empty;
    defer buf.deinit(alloc);

    const D = struct {
        sum: u16,

        pub fn visit(self: *@This(), x: []const u16) bool {
            self.sum += x[0] + x[1];
            return false;
        }
    };

    var ctx = D{ .sum = 0 };
    _ = try combinations(u16, alloc, &items, 2, &buf, &ctx, D.visit);
    try std.testing.expectEqual(30, ctx.sum);
}

test "combinations with reuse" {
    const alloc = std.testing.allocator;

    const items = [_]u16{ 1, 2, 3 };
    var buf: std.ArrayList(u16) = .empty;
    defer buf.deinit(alloc);

    const D = struct {
        sum: u16,

        pub fn visit(self: *@This(), x: []const u16) bool {
            self.sum += x[0] + x[1];
            return false;
        }
    };

    var ctx = D{ .sum = 0 };
    _ = try combinationsWithReuse(u16, alloc, &items, 2, &buf, &ctx, D.visit);
    try std.testing.expectEqual(24, ctx.sum);
}
