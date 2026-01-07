const std = @import("std");
const util = @import("util.zig");

const MAX_LEN = 10;

const Machine = struct {
    target_state: u10,
    buttons: std.ArrayList(u10) = .empty,
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
        const n = std.fmt.parseInt(u4, char, 10) catch unreachable;
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

fn pressButton(state: u10, button: u10) u10 {
    return state ^ button;
}

test "pressButton" {
    try std.testing.expectEqual(0b1000100011, pressButton(0, 0b1000100011));
    try std.testing.expectEqual(0, pressButton(0b101, 0b101));
}

fn parseMachine(alloc: std.mem.Allocator, line: []const u8) !Machine {
    var iter = std.mem.splitScalar(u8, line, ' ');
    const state = parseState(iter.next().?);
    var machine = Machine{
        .target_state = state,
    };

    while (iter.next()) |s| {
        const btn = parseButton(s) catch break;
        try machine.buttons.append(alloc, btn);
    }

    return machine;
}

fn callback(line: []const u8, ctx: *Context) !void {
    const machine = try parseMachine(ctx.alloc, line);
    try ctx.addMachine(machine);
}

fn combinations(
    comptime T: type,
    alloc: std.mem.Allocator,
    items: []const T,
    n: usize,
    start: usize,
    buf: *std.ArrayList(T),
    ctx: anytype,
    comptime CallbackFn: fn (@TypeOf(ctx), []const T) bool,
) !bool {
    if (buf.items.len == n) {
        return CallbackFn(ctx, buf.items);
    }

    var i = start;
    while (i <= items.len - (n - buf.items.len)) : (i += 1) {
        try buf.append(alloc, items[i]);
        defer _ = buf.pop();

        if (try combinations(T, alloc, items, n, i + 1, buf, ctx, CallbackFn))
            return true;
    }

    return false;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer std.debug.assert(gpa.deinit() == .ok);
    const alloc = gpa.allocator();

    var ctx = Context.init(alloc);
    defer ctx.deinit();

    try util.processFile(alloc, "input/day10", &ctx, callback, '\n');

    const Part1 = struct {
        button_presses: usize = 0,
        current_machine: ?Machine = null,

        pub fn visit(self: *@This(), buttons: []const u10) bool {
            var state: u10 = 0;
            for (buttons) |btn|
                state = pressButton(state, btn);

            if (self.current_machine.?.target_state == state) {
                self.button_presses += buttons.len;
                std.debug.print("  found after {d} button presses\n", .{buttons.len});
                for (buttons) |btn|
                    std.debug.print("    {b:0>10}\n", .{btn});
                return true;
            }

            return false;
        }
    };

    var part1 = Part1{};
    var buf: std.ArrayList(u10) = .empty;
    defer buf.deinit(alloc);

    outer: for (ctx.machines.items) |machine| {
        part1.current_machine = machine;
        var i: u8 = 0;
        while (true) : (i += 1) {
            std.debug.print("checking {d}-tuples...\n", .{i});
            std.debug.print("target state: {b:0>10}\n", .{machine.target_state});
            const found = try combinations(u10, alloc, machine.buttons.items, i, 0, &buf, &part1, Part1.visit);
            buf.clearAndFree(alloc);
            if (found) continue :outer;
        }
    }

    std.debug.print("Part 1: {d}\n", .{part1.button_presses});
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
    _ = try combinations(u16, alloc, &items, 2, 0, &buf, &ctx, D.visit);
    try std.testing.expectEqual(30, ctx.sum);
}
