const std = @import("std");
const lex = @import("lexer.zig");

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();

    defer _ = gpa.deinit();

    const argv = try std.process.argsAlloc(alloc);
    _ = argv;
    defer std.process.argsFree(alloc, argv);
    var tokens :[]lex.Token = undefined;
    if (argv.len > 1) {
        for (argv) |arg, i| { // No idea why, but this way its the safest so fuck it
            if (i == 0) continue;
            if (i == 1) tokens = try lex.tokenize(arg, alloc);
            if (i == 2) break;
        }
    // XXX for now i leave it like this so it doesn't cause a leak :P
     try lex.freeTokenValues(tokens,alloc);
     alloc.free(tokens);
    } else {
        std.log.info("No file provided :/", .{});
    }
}
