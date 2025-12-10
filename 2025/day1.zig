const std = @import("std");

const Dir = enum {
    left,
    right,
};

fn rotateDial(pos: i16, dir: Dir, offset: i16) i16 {
    if (dir == .left) {
        return pos - offset;
    } else {
        return pos + offset;
    }
}

fn countZeroHits() !usize {
    var gpa = std.heap.DebugAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    const input = try std.fs.cwd().openFile("input/day1", .{ .mode = .read_only });
    defer input.close();
    var buf: [2]u8 = undefined;
    var reader: std.fs.File.Reader = input.reader(&buf);

    var line = std.Io.Writer.Allocating.init(alloc);
    defer line.deinit();

    var count: usize = 0;
    var pos: i16 = 50;
    var dir: Dir = undefined;
    while (true) {
        _ = reader.interface.streamDelimiter(&line.writer, '\n') catch |err| {
            if (err == error.EndOfStream) break else return err;
        };
        _ = reader.interface.toss(1);
        const l = line.written();
        const offset = try std.fmt.parseInt(i16, l[1..], 10);
        if (l[0] == 'L') {
            dir = .left;
        } else {
            dir = .right;
        }
        const new_pos = rotateDial(pos, dir, offset);
        pos = new_pos;
        if (@mod(new_pos, 100) == 0) {
            count += 1;
        }
        std.debug.print("{d}\n", .{new_pos});
        line.clearRetainingCapacity();
    }

    return count;
}

test "part 1" {
    const result = countZeroHits();
    try std.testing.expectEqual(1147, result);
}
