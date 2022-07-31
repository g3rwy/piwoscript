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

    ID,FUNC_CALL,PARAMETERS,FIELD_ACCESS,ARR_ACCESS,

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
    NotMatch, // used to determine if function should go further 
    IncorrectEqualOp,
    UnclosedParenExpr,
    UnclosedParenFunc,
    UnclosedBrackArr,
    NoIDFieldAccess,
};

pub const FilePre = struct {
    tokens : [] const lex.Token,
    ast : *Node    
};

pub fn parseFile(name: []const u8, alloc: Allocator) !FilePre {
    var buffer = try readFileToString(name, alloc);
    
    var tokens = try lex.tokenize(buffer,alloc);
    alloc.free(buffer);

    
    
    if(parse(tokens,alloc)) |res|{
        return FilePre{ .tokens = tokens, .ast =  res};
    }else |err| {
        // XXX free all the tokens if they are not freed in parse()
        alloc.free(tokens);
        return err;
    }
}


fn func_params(alloc: Allocator, idx: *usize,tok_list: []const lex.Token) !Node {
    var func = Node{.typ = .PARAMETERS ,.children = ArrayList(Node).init(alloc)};
    errdefer freeNodesValues(func,alloc);
    var first : bool = true;
    
    // did it so its not considered error when you by mistake do foo(,5) or foo(2,3,)
    while(true){
        if(tok_list[idx.*].tok == Tok_enum.COMMA or first){
            if(tok_list[idx.*].tok == Tok_enum.COMMA) idx.* += 1;
            
            if(and_or(alloc,idx,tok_list,null)) |exp| {
                try func.children.append(exp);
            }
            else |err| {
                if(err != ParserError.NotMatch) return err;
                break;
            }
            first = false;
        }
        else { break; }
    } 
    return func;
}

fn field_arr(alloc: Allocator, idx: *usize,tok_list: []const lex.Token, lh : Node) !Node {
    var res : Node = Node{.typ = .PROGRAM ,.children = ArrayList(Node).init(alloc)};
    try res.children.append(lh);
    
    errdefer res.children.deinit();

    if(tok_list[idx.*].tok == Tok_enum.PERIOD) {
        res.typ = .FIELD_ACCESS;
        idx.* += 1;
        if(tok_list[idx.*].tok == Tok_enum.IDENTIFIER){
            var id = Node{.typ = .ID ,.children = ArrayList(Node).init(alloc), .value = tok_list[idx.*].value.? };
            errdefer id.children.deinit();
            idx.* += 1;

            try res.children.append(id);
            if(tok_list[idx.*].tok == Tok_enum.L_BRACK or tok_list[idx.*].tok == Tok_enum.PERIOD){
               return field_arr(alloc,idx,tok_list,res);
            }
            else{ return res; }
        }
        
        else{ return ParserError.NoIDFieldAccess; }
    }
    else if(tok_list[idx.*].tok == Tok_enum.L_BRACK) {
        res.typ = .ARR_ACCESS;        
        idx.* += 1;
        if(and_or(alloc,idx,tok_list,null)) |exp| {
            try res.children.append(exp);
        }
        else |err| {
            if(err != ParserError.NotMatch) return err;
        }
        if(tok_list[idx.*].tok == Tok_enum.R_BRACK){
            idx.* += 1;
            if(tok_list[idx.*].tok == Tok_enum.L_BRACK or tok_list[idx.*].tok == Tok_enum.PERIOD){
                return field_arr(alloc,idx,tok_list,res);
            }
            else{ return res; }
        }
        // if there is no ] at the end of expression
        else{ return ParserError.UnclosedBrackArr; }
    }
    
    return ParserError.NotMatch;
}


fn atom(alloc: Allocator, idx: *usize,tok_list: []const lex.Token) !Node {
    switch(tok_list[idx.*].tok){
        .BOOL_LIT,.STRING_LIT,.CHAR_LIT,.INT_LIT,.FLOAT_LIT => {
            // checking for literals
            var lit = Node{.typ = @intToEnum(NodeType,@enumToInt(tok_list[idx.*].tok) - @enumToInt(Tok_enum.STRING_LIT) + @enumToInt(NodeType.STRING_LIT)),.children = ArrayList(Node).init(alloc)};
            lit.value = tok_list[idx.*].value.?;
            idx.* += 1;
            return lit;
        },
        .IDENTIFIER => {
             var id = Node{.typ = .ID ,.children = ArrayList(Node).init(alloc), .value = tok_list[idx.*].value.? };
             idx.* += 1;
             const tok = tok_list[idx.*].tok;
             errdefer id.children.deinit();
             switch(tok){
                Tok_enum.L_PAREN => { // func call
                    // errdefer freeNodesValues(id,alloc);
                    idx.* += 1;
                    var func = Node{.typ = .FUNC_CALL ,.children = ArrayList(Node).init(alloc)};
                    errdefer func.children.deinit();
                    var params = try func_params(alloc,idx,tok_list);
                    if(tok_list[idx.*].tok == Tok_enum.R_PAREN){
                        // --MAYBE FIXED-- FIXME, might cause a double free, if id is appended to func and cause an error
                        idx.* += 1;
                        try func.children.append(id);
                        try func.children.append(params);
                        return func;
                    }
                    
                    freeNodesValues(params,alloc);
                    return ParserError.UnclosedParenFunc;
                },
                Tok_enum.PERIOD,Tok_enum.L_BRACK => {    
                    return field_arr(alloc, idx, tok_list, id);
                },
                else => {}
             }
                return id;
        },
        else => {
            // if its nothing other, check for expression in parenthesis   
            if(tok_list[idx.*].tok == Tok_enum.L_PAREN){
                idx.* += 1;
                var exp = try and_or(alloc,idx,tok_list,null);
                // if it returns ParserError then its okay, since there should be expression here, if anything else then good too since we don't want errors
                if(tok_list[idx.*].tok == Tok_enum.R_PAREN){ idx.* += 1;return exp; }
                else{ 
                    freeNodesValues(exp,alloc); // if we return error we need to free all the children of node
                    return ParserError.UnclosedParenExpr;
                    }
            }   
        }
    }
    return ParserError.NotMatch;
}

fn unary(alloc: Allocator, idx: *usize,tok_list: []const lex.Token) anyerror!Node {
    const symbol : ?Tok_enum = if(tok_list[idx.*].tok == Tok_enum.NOT or tok_list[idx.*].tok == Tok_enum.SUB) tok_list[idx.*].tok else null;
    if(symbol == null){ // if no symbol at front, then it must be atom
        return atom(alloc,idx,tok_list);
    }
    else{
        var un = Node{.typ = if(symbol == Tok_enum.NOT) .NOT else .NEG,.children = ArrayList(Node).init(alloc)};
        idx.* += 1;
        errdefer un.children.deinit();
        try un.children.append(try unary(alloc,idx,tok_list));
        return un;
    }
    return ParserError.NotMatch;
}

fn factor(alloc: Allocator, idx: *usize,tok_list: []const lex.Token,lh : ?Node) !Node {
    var lhs : Node = undefined;
    if(lh == null) { lhs = try unary(alloc,idx,tok_list); } 
    else { lhs = lh.?; }
    // XXX use switch instead of if?
    const next_tok : ?Tok_enum = switch(tok_list[idx.*].tok){
        Tok_enum.DIV,Tok_enum.MUL,Tok_enum.MOD => tok_list[idx.*].tok,
        else => null
    };
    
    // no operator found after it
    if(next_tok == null){ return lhs; }
    else{
        var tree = Node{.typ = switch(next_tok.?){
            Tok_enum.DIV => .DIV,   
            Tok_enum.MUL => .MUL,   
            Tok_enum.MOD => .MOD,
            else => {return ParserError.NotMatch;}
        } ,.children = ArrayList(Node).init(alloc)};

        idx.* += 1;
        errdefer tree.children.deinit();

        const rhs = try unary(alloc,idx,tok_list);
        try tree.children.append(lhs);
        try tree.children.append(rhs);
        if(tok_list[idx.*].tok == Tok_enum.MUL or tok_list[idx.*].tok == Tok_enum.DIV or tok_list[idx.*].tok == Tok_enum.MOD) 
        { return factor(alloc,idx,tok_list,tree); }

        return tree;
    }

    return ParserError.NotMatch;
}

fn term(alloc: Allocator, idx: *usize,tok_list: []const lex.Token,lh : ?Node) !Node {
    var lhs : Node = undefined;
    if(lh == null) { lhs = try factor(alloc,idx,tok_list,null); } 
    else { lhs = lh.?; }
    const next_tok : ?Tok_enum = if(tok_list[idx.*].tok == Tok_enum.ADD or tok_list[idx.*].tok == Tok_enum.SUB) tok_list[idx.*].tok else null;
    
    // no operator found after it
    if(next_tok == null){ return lhs; }
    else{
        var tree = Node{.typ = if(next_tok.? == Tok_enum.ADD) .ADD else .SUB ,.children = ArrayList(Node).init(alloc)};
        idx.* += 1;
        errdefer {
            // FIXME Current problem is that rhs returns error and some node in lhs doesn't get deinit()
            freeNodesValues(lhs,alloc);
            tree.children.deinit();
        }
        try tree.children.append(lhs);
        const rhs = try factor(alloc,idx,tok_list,null);
        try tree.children.append(rhs);
        if(tok_list[idx.*].tok == Tok_enum.ADD or tok_list[idx.*].tok == Tok_enum.SUB) { return term(alloc,idx,tok_list,tree); }

        return tree;
    }
    return ParserError.NotMatch;
}

// Tree when everything is alrighty, the last ADD with 2 and 2 is one that errors out
// - PROGRAM
    // - EXPR
        // - ADD
            // - SUB
                // - ADD
                    // - ID a
                    // - INT_LIT 5
                // - ARR_ACCESS
                    // - ID foo
                    // - INT_LIT 1
            // - ADD
                // - INT_LIT 2
                // - INT_LIT 2

fn comparison(alloc: Allocator, idx: *usize,tok_list: []const lex.Token,lh: ?Node) !Node {
    var lhs : Node = undefined;
    if(lh == null) { lhs = try term(alloc,idx,tok_list,null); } 
    else { lhs = lh.?; }
    const next_tok : ?Tok_enum = if(tok_list[idx.*].tok == .BIGGER or tok_list[idx.*].tok == .SMALLER) tok_list[idx.*].tok else null;

    // no operator found after it
    if(next_tok == null){ return lhs; }
    else{
        var tree : Node = undefined;
        if(tok_list[idx.* + 1].tok == .EQU){ // its >= or <=
            tree = Node{.typ = if(next_tok.? == .BIGGER) .MORE_E else .LESS_E ,.children = ArrayList(Node).init(alloc)};
            idx.* += 2;
        }
        else{ // its > or < 
            tree = Node{.typ = if(next_tok.? == .BIGGER) .MORE else .LESS ,.children = ArrayList(Node).init(alloc)};
            idx.* += 1;
        }

        errdefer {
            std.debug.print("FUCKING",.{});
            tree.children.deinit();
        }
        const rhs = try term(alloc,idx,tok_list,null);
        try tree.children.append(lhs);
        try tree.children.append(rhs);
        if(tok_list[idx.*].tok == .BIGGER or tok_list[idx.*].tok == .SMALLER) { return comparison(alloc,idx,tok_list,tree); }
        
        return tree;
    }
    return ParserError.NotMatch;
}

fn equality(alloc: Allocator, idx: *usize,tok_list: []const lex.Token,lh: ?Node) !Node {
    var lhs : Node = undefined;
    if(lh == null) { lhs = try comparison(alloc,idx,tok_list,null); } 
    else { lhs = lh.?; }
    const next_tok : ?Tok_enum = if(tok_list[idx.*].tok == .EQU or tok_list[idx.*].tok == Tok_enum.NOT) tok_list[idx.*].tok else null;

    // no operator found after it
    if(next_tok == null){ return lhs; }
    else{
        if(tok_list[idx.* + 1].tok == .EQU){
            // we initalize our logic node which is either == or != depends on the token and we add lhs and rhs as its two children nodes
            var tree = Node{.typ = if(next_tok.? == .EQU) .EQUAL else .N_EQUAL ,.children = ArrayList(Node).init(alloc)};
            idx.* += 2;
            
            errdefer tree.children.deinit();
            const rhs = try comparison(alloc,idx,tok_list,null);
            try tree.children.append(lhs);
            try tree.children.append(rhs);
            if(tok_list[idx.*].tok == .EQU or tok_list[idx.*].tok == Tok_enum.NOT) { return equality(alloc,idx,tok_list,tree); }


            return tree;
        }
        else{ return ParserError.IncorrectEqualOp; }
    }
    return ParserError.NotMatch;
} 

fn and_or(alloc: Allocator, idx: *usize,tok_list: []const lex.Token, lh: ?Node) anyerror!Node {
    var lhs : Node = undefined;
    if(lh == null) { lhs = try equality(alloc,idx,tok_list,null); }
    else { lhs = lh.?; }
    const next_tok : ?Tok_enum = if(tok_list[idx.*].tok == Tok_enum.AND or tok_list[idx.*].tok == Tok_enum.OR) tok_list[idx.*].tok else null;
    // no operator found after it
    if(next_tok == null){
        return lhs;
    }
    else{
        // we initalize our logic node which is either == or != depends on the token and we add lhs and rhs as its two children nodes
        var tree = Node{.typ = if(next_tok.? == Tok_enum.AND) .AND else .OR ,.children = ArrayList(Node).init(alloc)};
        idx.* += 1;
        
        errdefer tree.children.deinit();
        const rhs = try equality(alloc,idx,tok_list,null);
        try tree.children.append(lhs);
        try tree.children.append(rhs);

        // we check for operators after expression ( AND and OR keywords)
        // if operator is found, we know we gotta go further so we pass the expression as our left side and do everything again
        if(tok_list[idx.*].tok == Tok_enum.AND or tok_list[idx.*].tok == Tok_enum.OR) { return and_or(alloc,idx,tok_list,tree); }
        return tree;
    }
    return ParserError.NotMatch;
}

fn expression(alloc: Allocator, idx: *usize,tok_list: []const lex.Token) !Node {
    var exp = Node{.typ = .EXPR, .children = ArrayList(Node).init(alloc)};
    errdefer freeNodesValues(exp,alloc); // should have free all the nodes that have been added
    try exp.children.append(try and_or(alloc,idx,tok_list,null));
    return exp;
}


fn parse(tok_list: []const lex.Token, alloc: Allocator) !*Node {
    var Ast :*Node = try alloc.create(Node);
    Ast.* = Node{.typ = .PROGRAM, .children = ArrayList(Node).init(alloc)};
    var idx : usize = 0;
    errdefer {
        freeNodesValues(Ast.*,alloc);
        alloc.destroy(Ast);
        lex.freeTokenValues(tok_list,alloc,0); // TODO in future use token to print out where the error is and only then free it
    }
    try Ast.children.append(try expression(alloc,&idx,tok_list));
    return Ast;
}


pub fn freeAST(tokens: []const lex.Token,res: *Node,alloc: Allocator) void {
    lex.freeTokenValues(tokens,alloc,0);
    alloc.free(tokens);
    freeNodesValues(res.*,alloc);     // free node (the AST) all its children and their values
    alloc.destroy(res);
}

pub fn freeNode(res: *Node,alloc: Allocator) void {
    freeNodesValues(res.*,alloc);
    alloc.destroy(res);
}

// TODO remove all the alloc param from freeNodesValues
fn freeNodesValues(node: Node, alloc: Allocator) void {
    //if(node.value != null)    alloc.free(node.value.?);
    for(node.children.items) |c| 
           freeNodesValues(c,alloc);
    node.children.deinit();
}

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
    if(true) return error.SkipZigTest;
    var tokens = try lex.tokenize("population.city[5]", test_alloc);
    var res    = try parse(tokens, test_alloc);
    defer    freeAST(tokens,res,test_alloc);
    // try printNodes(res.*,0);
}

test "par unclosed parenthesis for expression" {
    // if(true) return error.SkipZigTest;
    var tokens = try lex.tokenize("a[69] + (2 + 2", test_alloc);
    if(parse(tokens, test_alloc)) |res|{
        freeAST(tokens,res,test_alloc);
        unreachable;
    }else |err| {
        test_alloc.free(tokens);
        try expect(err == ParserError.UnclosedParenExpr);
    }
}

test "par unclosed parenthesis for function" {
    if(true) return error.SkipZigTest;
    var tokens = try lex.tokenize("foo(69", test_alloc);
    if(parse(tokens, test_alloc)) |res|{
        freeAST(tokens,res,test_alloc);
        unreachable;
    }else |err| {
        test_alloc.free(tokens);
        try expect(err == ParserError.UnclosedParenFunc);
    }
}

test "par parsing a file instead of const string" {
    if(true) return error.SkipZigTest;
    var res = try parseFile("exp.piwo",test_alloc);
    try printNodes(res.ast.*,0);
    freeAST(res.tokens,res.ast,test_alloc);
}
