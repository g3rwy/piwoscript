const std = @import("std");
const lex = @import("lexer.zig");
const parser = @import("parser.zig"); 

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();

    defer _ = gpa.deinit();

    const argv = try std.process.argsAlloc(alloc);
    _ = argv;
    defer std.process.argsFree(alloc, argv);
    var AST : *parser.Node = undefined;

    
    if (argv.len > 1) {
        for (argv) |arg, i| { // No idea why, but this way its the safest so fuck it
            if (i == 1) AST = try parser.parseFile(arg, alloc);
        }
    // XXX for now i leave it like this so it doesn't cause a leak :P
    try parser.printNodes(AST.*,0);
    parser.freeNode(AST,alloc);
    
    } else {
        const stdout = std.io.getStdOut().writer();
        try stdout.print("No file provided :/\n", .{});
    }
}
