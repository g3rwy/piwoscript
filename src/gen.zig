const std = @import("std");
const Node = @import("parser.zig").Node;

pub fn generate_c(ast: std.ArrayList(Node), buffer: std.ArrayList(u8).Writer) !void{
    _ = ast;
    const source = \\
        \\#include "string.h"
        \\int main(){
        \\    int temp_gc;
        \\    {
        \\        tgc_start(&gc,&temp_gc);
        \\        printf("Hello from from Piwo\n");
        \\        vec_t(char) alloc_string;
        \\        vec_init(&alloc_string);
        \\        vec_reserve(&alloc_string,16);
        \\        strcpy(alloc_string.data,"CHUJ ąć");
        \\        printf("%s\n",alloc_string);
        \\        
        \\    }
        \\    tgc_stop(&gc);
        \\}
    ;
   try buffer.writeAll(source);
}