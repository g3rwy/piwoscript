const std = @import("std");
const lex = @import("lexer.zig");
const parser = @import("parser.zig"); 
const analyzer = @import("anal.zig");
const generator = @import("gen.zig");
const tcc = @cImport({
   @cInclude("libtcc.h"); 
});

const headers = @embedFile("runtime/tgc.h") ++ @embedFile("runtime/vec.h");

var errwarn: u32 = 0;
pub fn errorFunc(data: ?*anyopaque , string: [*c]const u8) callconv(.C) void {
    _ = data;
    std.debug.print("PIWO: {s}\n", .{string});
    errwarn += 1;
}

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();

    defer _ = gpa.deinit();
    errdefer _ = gpa.deinit();

    const argv = try std.process.argsAlloc(alloc);
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
    // try parser.printNodes(Ast.ast,0,0);
        
    try analyzer.analyze(Ast.ast);
    var buf = std.ArrayList(u8).init(alloc);
    try buf.appendSlice(headers); // FIXME Not a good thing to compile all of this with every run :/ so figure out a way to compile it to lib and link to it

    try generator.generate_c(Ast.ast,buf.writer());
    // If set, run it or compile it to file 
    
    // std.debug.print("{s}\n",.{buf.items});
    
    //Compiling C from buffer
        var state : *tcc.TCCState = undefined;
        state = tcc.tcc_new().?;
        tcc.tcc_set_error_func(state,null,&errorFunc);
        
        _ = tcc.tcc_set_output_type(state,tcc.TCC_OUTPUT_MEMORY);
        try buf.append(0); // null byte for C string
        if(tcc.tcc_compile_string(state, buf.items.ptr) == -1){
            try stdout.print("Errors/Warnings: {d}",.{errwarn});
            @panic("Failed compilation\n");
        }
        _ = tcc.tcc_run(state,0,0);
    
    tcc.tcc_delete(state);
    // =====================================
        
    buf.deinit();
    parser.freeAST(Ast.tokens,Ast.ast,alloc);    
    } else {
        try stdout.print("No input provided :/\n", .{});
    }
}
