const std = @import("std");
const Node = @import("parser.zig").Node;

pub fn generate_c(ast: std.ArrayList(Node), buffer: std.ArrayList(u8).Writer) !void{
    _ = ast;
   try buffer.writeAll("Henlo :3");
}