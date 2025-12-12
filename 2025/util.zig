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
        _ = reader.interface.streamDelimiter(&line.writer, delim) catch |err| {
            if (err == error.EndOfStream) break else return err;
        };
        _ = reader.interface.toss(1);

        const slice = line.written();
        try CallbackFn(slice, ctx);

        line.clearRetainingCapacity();
    }
}
