const Node = @import("parser.zig").Node;
const std = @import("std");

//TODO
//- Check if ELSE has IF node before it
//- Transform UTF-8 polish letters to unused printable ascii ones
//- Check for correct expressions (if types are correct)
//- Transform ID's to indexes
//- Check for simple Division by 0

pub fn analyze(ast: std.ArrayList(Node)) !void {
    _ = ast;
}