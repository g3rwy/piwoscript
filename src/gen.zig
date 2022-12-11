const std = @import("std");
const Node = @import("parser.zig").Node;
const get_type = @import("anal.zig").get_expr_type;
const NodeType = @import("parser.zig").NodeType;

pub fn generate_c(ast: std.ArrayList(Node), buffer: std.ArrayList(u8).Writer) !void{
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    defer  _ = gpa.deinit();

    const head_source = \\
        \\#include "string.h"
        \\#include "stdlib.h"
        \\int main(){
        \\    int temp_gc;
        \\    {
        \\        tgc_start(&gc,&temp_gc);
    ;
    try buffer.writeAll(head_source);

    const bottom_source =
        \\    }
        \\    tgc_stop(&gc);
        \\}
    ;
    var gen_source = try std.ArrayList(u8).initCapacity(alloc, 32);

    const root = ast.items[0];

    for(root.children.items) |stat_idx| {
        var statement = ast.items[stat_idx];
        if(statement.typ == .PRINT){
            const expr_result = get_expr(ast,alloc, ast.items[statement.children.items[0]] );
            // NOTE FIXME Count for custom prints in future, like array and shit
            // maybe with return type check it in switch and use different function than printf
            try gen_source.writer().print(
                "printf(\"%{c}\",{s});",
                .{ expr_result.typ, expr_result.result },
                );
            try buffer.writeAll(gen_source.items);
        }
    }

    std.debug.print("\n== {s} ==\n",.{gen_source.items});
    gen_source.deinit();

    try buffer.writeAll(bottom_source);
}
//TODO make it spit out the type of expression and string version of it with runtime functions instead of operators
fn get_expr(ast: std.ArrayList(Node), alloc: std.mem.Allocator, exp: Node) struct {typ: u8,result: []const u8} {
    _ = ast;
    _ = alloc;
    _ = exp;
    return .{.typ = 'd' , .result = "69"};
}
