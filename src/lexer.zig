const std = @import("std");

fn readFileToString(path: []const u8, allocator: std.mem.Allocator) ![]u8 {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    const expected_max_size = 2_000_000;
    return try file.reader().readAllAlloc(allocator, expected_max_size);
}

pub fn tokenize(fname: []const u8, alloc: std.mem.Allocator) !void {
    var buf = try readFileToString(fname, alloc);
    defer alloc.free(buf);

    std.debug.print("{s}\n", .{buf});
}
