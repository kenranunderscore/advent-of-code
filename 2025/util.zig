const std = @import("std");

pub fn processLines(
    lines: []const []const u8,
    ctx: anytype,
    comptime CallbackFn: fn ([]const u8, @TypeOf(ctx)) anyerror!void,
) !void {
    for (lines) |line| {
        try CallbackFn(line, ctx);
    }
}

pub fn processFile(
    alloc: std.mem.Allocator,
    filename: []const u8,
    ctx: anytype,
    comptime CallbackFn: fn ([]const u8, @TypeOf(ctx)) anyerror!void,
    delim: u8,
) !void {
    const input = try std.fs.cwd().openFile(filename, .{ .mode = .read_only });
    defer input.close();

    var buf: [100]u8 = undefined;
    var reader = input.reader(&buf);
    var line = std.io.Writer.Allocating.init(alloc);
    defer line.deinit();

    while (true) {
        var eof = false;

        _ = reader.interface.streamDelimiter(&line.writer, delim) catch |err| switch (err) {
            error.EndOfStream => eof = true,
            else => return err,
        };

        const slice = line.written();
        if (slice.len > 0) {
            try CallbackFn(slice, ctx);
            // TODO(Johannes): test this
            line.clearRetainingCapacity();
        }

        if (eof) break;

        _ = reader.interface.toss(1);
    }
}

const TestContext = struct {
    alloc: std.mem.Allocator = std.testing.allocator,
    lines: std.ArrayList([]const u8),

    fn deinit(self: *@This()) void {
        for (self.lines.items) |line| {
            self.alloc.free(line);
        }
        self.lines.deinit(self.alloc);
    }
};

fn testCallback(line: []const u8, ctx: *TestContext) !void {
    const owned = try ctx.alloc.dupe(u8, line);
    try ctx.lines.append(ctx.alloc, owned);
}

test "finds chunk after last delimiter and before eof" {
    const filename = "test_file";
    const file = try std.fs.cwd().createFile(filename, .{ .read = true });
    defer file.close();

    var buf: [100]u8 = undefined;
    var writer = file.writer(&buf);
    try writer.interface.writeAll("first\nsecond\nwithout newline");
    try writer.end();

    var ctx = TestContext{ .lines = std.ArrayList([]const u8).empty };
    defer ctx.deinit();
    try processFile(
        std.testing.allocator,
        filename,
        &ctx,
        testCallback,
        '\n',
    );

    const expected = [_][]const u8{ "first", "second", "without newline" };
    try std.testing.expectEqual(expected.len, ctx.lines.items.len);
    for (expected, ctx.lines.items) |exp, actual| {
        try std.testing.expectEqualStrings(exp, actual);
    }

    try std.fs.cwd().deleteFile(filename);
}
