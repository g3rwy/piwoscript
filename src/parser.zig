const std = @import("std");
const lex = @import("lexer.zig");
const readFileToString = @import("utils.zig").readFileToString;

const Tok_enum = lex.Tok_enum;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

pub const NodeType = enum {
    PROGRAM, 
    STATEMENT,
    
    STRING_LIT,INT_LIT,CHAR_LIT,FLOAT_LIT,BOOL_LIT,
    ADD,SUB,MUL,MOD,DIV,AND,OR,NOT,NEG,
    EQUAL,N_EQUAL,MORE,LESS,MORE_E,LESS_E,

    ID,

    IF,WHILE,PRINT,INPUT,FOREACH,
    VAR_DECL,CONST_DECL,ARR_DECL,ASSIGN,
    EXPR,
};

pub const Node  = struct {
      typ: NodeType,
      value: ?[]const u8 = null,
      children: ArrayList(Node)
};

pub const ParserError = error {
    NotMatch, // used to determine if function should go further and if the statment or expression is correct
    StringIntOperation,
    UndeclaredIdentifier,
    DivideByZero,
    IncorrectEqualOp,    
};

pub fn parseFile(name: []const u8, alloc: Allocator) !*Node {
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

// okay so this parsing doesn't really work
fn atom(alloc: Allocator, idx: *usize,tok_list: []const lex.Token, right: bool) !Node {
    if(right)   {
        return and_or(alloc,idx,tok_list);
    }
    switch(tok_list[idx.*].tok){
        .BOOL_LIT,.STRING_LIT,.CHAR_LIT,.INT_LIT,.FLOAT_LIT => {
            // checking for literals
            var lit = Node{.typ = @intToEnum(NodeType,@enumToInt(tok_list[idx.*].tok) - @enumToInt(Tok_enum.STRING_LIT) + @enumToInt(NodeType.STRING_LIT)),.children = ArrayList(Node).init(alloc)};
            lit.value = try alloc.dupe(u8,tok_list[idx.*].value.?);
            idx.* += 1;
            return lit;
        },
        .IDENTIFIER => {
            var id = Node{.typ = .ID ,.children = ArrayList(Node).init(alloc)};
            id.value = try alloc.dupe(u8,tok_list[idx.*].value.?);
            idx.* += 1;
            return id;
            // check for method_call
            // check for func_call
            // check for arr_access
            // otherwise its just id
        },
        else => {
            // if its nothing other, check for expression            
        }
    }
    return ParserError.NotMatch;
}
// TODO for it to work, it needs to know if its on right side, so we gonna pass the bools

fn unary(alloc: Allocator, idx: *usize,tok_list: []const lex.Token, right: bool) !Node {
    const symbol : ?Tok_enum = if(tok_list[idx.*].tok == Tok_enum.NOT or tok_list[idx.*].tok == Tok_enum.SUB) tok_list[idx.*].tok else null;
    if(symbol == null){ // if no symbol at front, then it must be atom
        return atom(alloc,idx,tok_list,right);
    }
    else{
        var un = Node{.typ = if(symbol == Tok_enum.NOT) .NOT else .NEG,.children = ArrayList(Node).init(alloc)};
        idx.* += 1;
        try un.children.append(try atom(alloc,idx,tok_list,right));
        return un;
    }
    return ParserError.NotMatch;
}

fn factor(alloc: Allocator, idx: *usize,tok_list: []const lex.Token, right: bool) !Node {
    const lhs = try unary(alloc,idx,tok_list,right);
    // XXX use switch instead of if?
    const next_tok : ?Tok_enum = switch(tok_list[idx.*].tok){
        Tok_enum.DIV,Tok_enum.MUL,Tok_enum.MOD => tok_list[idx.*].tok,
        else => null
    };
    
    // no operator found after it
    if(next_tok == null){ return lhs; }
    else{
        var fact = Node{.typ = switch(next_tok.?){
            Tok_enum.DIV => .DIV,   
            Tok_enum.MUL => .MUL,   
            Tok_enum.MOD => .MOD,
            else => {return ParserError.NotMatch;}
        } ,.children = ArrayList(Node).init(alloc)};

        idx.* += 1;
        const rhs = try unary(alloc,idx,tok_list,true);
        try fact.children.append(lhs);
        try fact.children.append(rhs);

        return fact;
    }
    _= right;

    return ParserError.NotMatch;
}

fn term(alloc: Allocator, idx: *usize,tok_list: []const lex.Token, right: bool) !Node {
    const lhs = try factor(alloc,idx,tok_list,right);
    const next_tok : ?Tok_enum = if(tok_list[idx.*].tok == Tok_enum.ADD or tok_list[idx.*].tok == Tok_enum.SUB) tok_list[idx.*].tok else null;
    
    // no operator found after it
    if(next_tok == null){ return lhs; }
    else{
        var ter = Node{.typ = if(next_tok.? == Tok_enum.ADD) .ADD else .SUB ,.children = ArrayList(Node).init(alloc)};
        idx.* += 1;
        const rhs = try factor(alloc,idx,tok_list,true);
        try ter.children.append(lhs);
        try ter.children.append(rhs);

        return ter;
    }
    _= right;
    return ParserError.NotMatch;
}

fn comparison(alloc: Allocator, idx: *usize,tok_list: []const lex.Token, right: bool) !Node {
    const lhs = try term(alloc,idx,tok_list,right);
    const next_tok : ?Tok_enum = if(tok_list[idx.*].tok == .BIGGER or tok_list[idx.*].tok == .SMALLER) tok_list[idx.*].tok else null;

    // no operator found after it
    if(next_tok == null){ return lhs; }
    else{
        var comp : Node = undefined;
        if(tok_list[idx.* + 1].tok == .EQU){ // its >= or <=
            comp = Node{.typ = if(next_tok.? == .BIGGER) .MORE_E else .LESS_E ,.children = ArrayList(Node).init(alloc)};
            idx.* += 2;
        }
        else{ // its > or < 
            comp = Node{.typ = if(next_tok.? == .BIGGER) .MORE else .LESS ,.children = ArrayList(Node).init(alloc)};
            idx.* += 1;
        }

        const rhs = try term(alloc,idx,tok_list,true);
        try comp.children.append(lhs);
        try comp.children.append(rhs);
        
        return comp;
    }
    _= right;
    return ParserError.NotMatch;
}

fn equality(alloc: Allocator, idx: *usize,tok_list: []const lex.Token, right: bool) !Node {
    const lhs = try comparison(alloc,idx,tok_list,right);
    const next_tok : ?Tok_enum = if(tok_list[idx.*].tok == .EQU or tok_list[idx.*].tok == Tok_enum.NOT) tok_list[idx.*].tok else null;

    // no operator found after it
    if(next_tok == null){ return lhs; }
    else{
        if(tok_list[idx.* + 1].tok == .EQU){
            // we initalize our logic node which is either == or != depends on the token and we add lhs and rhs as its two children nodes
            var logic = Node{.typ = if(next_tok.? == .EQU) .EQUAL else .N_EQUAL ,.children = ArrayList(Node).init(alloc)};
            idx.* += 2;
            
            const rhs = try comparison(alloc,idx,tok_list,true);
            try logic.children.append(lhs);
            try logic.children.append(rhs);
            
            return logic;
        }
        else{ return ParserError.IncorrectEqualOp; }
    }
    _= right;
    return ParserError.NotMatch;
} 

fn and_or(alloc: Allocator, idx: *usize,tok_list: []const lex.Token) anyerror!Node {
    const lhs = try equality(alloc,idx,tok_list,false);
    const next_tok : ?Tok_enum = if(tok_list[idx.*].tok == Tok_enum.AND or tok_list[idx.*].tok == Tok_enum.OR) tok_list[idx.*].tok else null;

    // no operator found after it
    if(next_tok == null){ return lhs; }
    else{
        // we initalize our logic node which is either == or != depends on the token and we add lhs and rhs as its two children nodes
        var andor = Node{.typ = if(next_tok.? == Tok_enum.AND) .AND else .OR ,.children = ArrayList(Node).init(alloc)};
        idx.* += 1;
        
        const rhs = try equality(alloc,idx,tok_list,true);
        try andor.children.append(lhs);
        try andor.children.append(rhs);
        
        return andor;
    }
    return ParserError.NotMatch;
}

fn expression(alloc: Allocator, idx: *usize,tok_list: []const lex.Token) !Node {
    var exp = Node{.typ = .EXPR, .children = ArrayList(Node).init(alloc)};
    
    _ = try exp.children.append(try and_or(alloc,idx,tok_list));
    return exp;
}


fn parse(tok_list: []const lex.Token, alloc: Allocator) !*Node {
    var Ast :*Node = try alloc.create(Node);
    Ast.* = Node{.typ = .PROGRAM, .children = ArrayList(Node).init(alloc)};
    var idx : usize = 0;
    try Ast.children.append(try expression(alloc,&idx,tok_list));
    
    return Ast;
}


pub fn freeAST(tokens: []const lex.Token,res: *Node,alloc: Allocator) void {
    lex.freeTokenValues(tokens,alloc); // free tokens and their values
    alloc.free(tokens);
    freeNodesValues(res.*,alloc);     // free node (the AST) all its children and their values
    alloc.destroy(res);
}

// My Dirty way of having it easier on me in main.zig
pub fn freeNode(res: *Node,alloc: Allocator) void {
    freeNodesValues(res.*,alloc);
    alloc.destroy(res);
}

fn freeNodesValues(node: Node, alloc: Allocator) void {
    if(node.value != null)    alloc.free(node.value.?);
    for(node.children.items) |c| 
           freeNodesValues(c,alloc);
    node.children.deinit();
}

//pub fn freeTokenValues(tokens : []const Token, alloc: Allocator) !void {
pub fn printNodes(node: Node, deep: u8) std.os.WriteError!void {
    const stdout = std.io.getStdOut().writer();
    if(deep == 0) try stdout.print("\n", .{});

    var i : u8 = deep;
    while(i != 0) : ( i -= 1){
        try stdout.print("    ", .{});
    }
    try stdout.print("- {s} {s}\n", .{@tagName(node.typ), if (node.value != null) node.value.? else ""});
    
    for(node.children.items) |n| {
        try printNodes(n, deep + 1);      
    }
}

const eql = std.mem.eql;
const expect = std.testing.expect;
const test_alloc = std.testing.allocator;

test "par expression" {
    var tokens = try lex.tokenize("6 / 3 - 1 == 1", test_alloc);
    var res    = try parse(tokens, test_alloc);
    defer    freeAST(tokens,res,test_alloc);
    errdefer freeAST(tokens,res,test_alloc);
    try printNodes(res.*,0);
}
