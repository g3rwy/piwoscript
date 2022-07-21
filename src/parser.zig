const std = @import("std");
const lex = @import("lexer.zig");
const readFileToString = @import("utils.zig").readFileToString;

const Tok_enum = lex.Tok_enum;
const ArrayList = std.ArrayList;

pub const NodeType = enum {
    PROGRAM, 
    STATEMENT,
    
    INT_LIT,FLOAT_LIT,STRING_LIT,CHAR_LIT,BOOL_LIT,
    ADD,SUB,MUL,MOD,DIV,AND,OR,NEG,
    EQUAL,N_EQUAL,MORE,LESS,MORE_E,LESS_E,

    ID,

    IF,WHILE,PRINT,INPUT,FOREACH,
    VAR_DECL,CONST_DECL,ARR_DECL,ASSIGN,
    EXPR, FACTOR, TERM, UNARY,
    EQUALITY,COMPARISON,
};

pub const Node  = struct {
      typ: NodeType,
      value: ?[]const u8 = null,
      children: ArrayList(Node)
};

pub const ParserError = error {
    
};

pub fn parseFile(name: []const u8, alloc: std.mem.Allocator) !*Node {
    const buffer = try readFileToString(name, alloc);
    defer alloc.free(buffer);
    const tokens = try lex.tokenize(buffer,alloc);
    defer {
            lex.freeTokenValues(tokens,alloc); // free tokens and their values
            alloc.free(tokens);
    }
    errdefer {
            lex.freeTokenValues(tokens,alloc); // free tokens and their values
            alloc.free(tokens);
    }
    return parse(tokens,alloc);
}

fn parse(tok_list: []const lex.Token, alloc: std.mem.Allocator) !*Node {
    var Ast :*Node = try alloc.create(Node);
    Ast.* = Node{.typ = .PROGRAM, .children = ArrayList(Node).init(alloc)};
    _ = tok_list;
    return Ast;
}


const eql = std.mem.eql;
const expect = std.testing.expect;
const test_alloc = std.testing.allocator;


pub fn freeAST(tokens: []const lex.Token,res: *Node,alloc: std.mem.Allocator) void {
    lex.freeTokenValues(tokens,alloc); // free tokens and their values
    alloc.free(tokens);
    freeNodesValues(res.*,alloc);     // free node (the AST) all its children and their values
    alloc.destroy(res);
}

// My Dirty way of having it easier on me in main.zig
pub fn freeNode(res: *Node,alloc: std.mem.Allocator) void {
    freeNodesValues(res.*,alloc);
    alloc.destroy(res);
}

fn freeNodesValues(node: Node, alloc: std.mem.Allocator) void {
    if(node.value != null)    alloc.free(node.value.?);
    for(node.children.items) |c| 
           freeNodesValues(c,alloc);
    node.children.deinit();
}

//pub fn freeTokenValues(tokens : []const Token, alloc: std.mem.Allocator) !void {
pub fn printNodes(node: Node, deep: u8) std.os.WriteError!void {
    const stdout = std.io.getStdOut().writer();
    if(deep == 0) try stdout.print("\n", .{});

    var i : u8 = deep;
    while(i != 0) : ( i -= 1){
        try stdout.print("    ", .{});
    }
    try stdout.print("- {s} {any}\n", .{@tagName(node.typ), if (node.value != null) node.value.? else ""});
    
    for(node.children.items) |n| {
        try printNodes(n, deep + 1);      
    }
}


test "par expression" {
    var tokens = try lex.tokenize("5 + 4", test_alloc);
    var res = try parse(tokens, test_alloc);
    defer    freeAST(tokens,res,test_alloc);
    errdefer freeAST(tokens,res,test_alloc);
    
    try printNodes(res.*,0);
}
