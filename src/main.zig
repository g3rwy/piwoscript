const std = @import("std");
const lex = @import("lexer.zig");

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();

    defer _ = gpa.deinit();

    const argv = try std.process.argsAlloc(alloc);
    _ = argv;
    defer std.process.argsFree(alloc, argv);

    if (argv.len > 1) {
        for (argv) |arg, i| { // No idea why, but this way its the safest so fuck it
            if (i == 0) continue;
            if (i == 1) _ = try lex.tokenize(arg, alloc);
            if (i == 2) break;
        }
    } else {
        std.log.info("No file provided :/", .{});
    }
}
