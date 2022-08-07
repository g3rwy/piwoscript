const std = @import("std");
const lex = @import("lexer.zig");
const parser = @import("parser.zig"); 

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();

    defer _ = gpa.deinit();
    errdefer _ = gpa.deinit();

    const argv = try std.process.argsAlloc(alloc);
    _ = argv;
    defer std.process.argsFree(alloc, argv);
    errdefer std.process.argsFree(alloc, argv);
    
    var Ast : parser.FilePre = undefined;

    const stdout = std.io.getStdOut().writer();
    
    if (argv.len > 1) {
        for (argv) |arg, i| {
            if (i == 1) {
            if(parser.parseFile(arg, alloc)) |result| { Ast = result; }
            else |err|{
                // TODO use switch and maybe like some kind of array of prompts that show off when error is there
                // Input is empty
                if(err == lex.LexerError.EmptyInput){
                    try stdout.print("The input is empty tho :/\n", .{});
                }
                return;
            
            }
            }
        }
    
    // XXX for now i leave it like this so it doesn't cause a leak :P
    try parser.printNodes(Ast.ast,0,0);
    parser.freeAST(Ast.tokens,Ast.ast,alloc);
    
    } else {
        try stdout.print("No input provided :/\n", .{});
    }
}
