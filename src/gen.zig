const std = @import("std");
const Node = @import("parser.zig").Node;

pub fn generate_c(ast: std.ArrayList(Node), buffer: std.ArrayList(u8).Writer) !void{
    _ = ast;
    const source = \\
        \\
        \\static tgc_t gc;
        \\int main(){
        \\    int temp_gc;
        \\    {
        \\        tgc_start(&gc,&temp_gc);
        \\        printf("Hello from from Piwo\n");
        \\        vec_int_t ints;
        \\        vec_init(&ints);
        \\        for(int i = 0; i < 10; i++){
        \\            vec_push(&ints,i+1);
        \\        }
        \\        for(int i = 0; i < 10; i++){
        \\            printf("%d\n",ints.data[i]);
        \\        }
        \\        vec_deinit(&ints);
        \\        tgc_stop(&gc);
        \\    }
        \\}
    ;
   try buffer.writeAll(source);
}