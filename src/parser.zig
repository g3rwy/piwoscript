const std = @import("std");
const lex = @import("lexer.zig");
const readFileToString = @import("utils.zig").readFileToString;

const Tok_enum = lex.Tok_enum;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
// const Array = std.BoundedArray;

const AVG_NODE_PER_LINE = 6;

pub const NodeType = enum {
    PROGRAM, 
    STATEMENT,
    
    STRING_LIT,INT_LIT,CHAR_LIT,FLOAT_LIT,BOOL_LIT,ARR_LIT,FIELD,
    STRING,INT,CHAR,FLOAT,BOOL,KURWA,
    ADD,SUB,MUL,MOD,DIV,AND,OR,NOT,NEG,
    
    EQUAL,N_EQUAL,MORE,LESS,MORE_E,LESS_E,

    ID,FUNC_CALL,PARAMETERS,FIELD_ACCESS,ARR_ACCESS,

    IF,WHILE,PRINT,INPUT,FOREACH,
    VAR_DECL,CONST_DECL,ARR_DECL,STRUCT_DECL,ASSIGN,
    EXPR,
};

pub const Node  = struct {
      typ: NodeType,
      value: ?[]const u8 = null,
      children: ArrayList(u32)
};

pub const ParserError = error {
    NotMatch, // used to determine if function should go further 
    IncorrectEqualOp,
    UnclosedParenExpr,
    UnclosedParenFunc,
    UnclosedBrackArr,
    NoIDFieldAccess,
    NoIDGiven,
    IncorrectType,
    NoAssignWhenNeeded,
    NoArrBrack,
    NoStructBrack,
    KurwaNotAllowed,
    NoIndentForBlock,
};

pub fn expectToken(tok : Tok_enum, tok_list: []const lex.Token,idx: usize) anyerror {
    var i :usize = 0;
    var line : usize = 1;
    while(i < tok_list.len and i <= idx) : (i += 1){          
        if(tok_list[i].tok == Tok_enum.NEWLINE){ line += 1; }    
    }
    
    if(@import("builtin").mode == .Debug){
        std.debug.print("Expected: {s} but got: {s}\nOn line {d}\n",.{tok,tok_list[idx].tok,line});
    }
    else{
        const stdout = std.io.getStdOut().writer();
        try stdout.print("Expected: {s} but got: {s}\nOn line {d}\n",.{tok,tok_list[idx].tok,line});
    }
    return ParserError.NotMatch;
}

pub const FilePre = struct {
    tokens : [] const lex.Token,
    ast : ArrayList(Node)    
};

pub fn parseFile(name: []const u8, alloc: Allocator) !FilePre {
    var buffer = try readFileToString(name, alloc);

    errdefer alloc.free(buffer);
    var tokens = try lex.tokenize(buffer,alloc);
    alloc.free(buffer);
    
    
    if(parse(tokens,alloc)) |res|{
        return FilePre{ .tokens = tokens, .ast =  res};
    }else |err| {
        return err;
    }
}


fn func_params(alloc: Allocator, idx: *usize,tok_list: []const lex.Token,ast : *ArrayList(Node)) !u32 {
    var par = Node{.typ = .PARAMETERS ,.children = ArrayList(u32).init(alloc)};
    errdefer par.children.deinit();
    var first : bool = true;
    try ast.append(par);
    const par_idx = @truncate(u32, ast.items.len - 1);
    
    // did it so its not considered error when you by mistake do foo(,5) or foo(2,3,)
    while(true){
        if(tok_list[idx.*].tok == Tok_enum.COMMA or first){
            if(tok_list[idx.*].tok == Tok_enum.COMMA) idx.* += 1;
            
            if(and_or(alloc,idx,tok_list,ast,null)) |exp| {
                try ast.items[par_idx].children.append(exp);
            }
            else |err| {
                if(err != ParserError.NotMatch) return err;
                break;
            }
            first = false;
        }
        else { break; }
    } 
    return par_idx;
}

fn field_arr(alloc: Allocator, idx: *usize,tok_list: []const lex.Token, lh : u32,ast : *ArrayList(Node)) !u32 {
    var res : Node = Node{.typ = .PROGRAM ,.children = ArrayList(u32).init(alloc)};
    errdefer res.children.deinit();
    

    

    if(tok_list[idx.*].tok == Tok_enum.PERIOD) {
        res.typ = .FIELD_ACCESS;
        idx.* += 1;
        try ast.append(res);
        const res_idx = @truncate(u32, ast.items.len - 1);
        try ast.items[res_idx].children.append(lh);

        if(tok_list[idx.*].tok == Tok_enum.IDENTIFIER){
            var id = Node{.typ = .ID ,.children = ArrayList(u32).init(alloc), .value = tok_list[idx.*].value.? };
            errdefer id.children.deinit();
            idx.* += 1;
            try ast.append(id);
            const id_idx = @truncate(u32, ast.items.len - 1);

            try ast.items[res_idx].children.append(id_idx);
            if(tok_list[idx.*].tok == Tok_enum.L_BRACK or tok_list[idx.*].tok == Tok_enum.PERIOD){
               return field_arr(alloc,idx,tok_list,res_idx,ast);
            }
            else{ return res_idx; }
        }
        
        else{ return ParserError.NoIDFieldAccess; }
    }
    else if(tok_list[idx.*].tok == Tok_enum.L_BRACK) {
        res.typ = .ARR_ACCESS;        
        idx.* += 1;
        try ast.append(res);
        const res_idx = @truncate(u32, ast.items.len - 1);
        try ast.items[res_idx].children.append(lh);

        if(and_or(alloc,idx,tok_list,ast,null)) |exp| {
            try  ast.items[res_idx].children.append(exp);
        }
        else |err| {
            if(err != ParserError.NotMatch) return err;
        }
        if(tok_list[idx.*].tok == Tok_enum.R_BRACK){
            idx.* += 1;
            if(tok_list[idx.*].tok == Tok_enum.L_BRACK or tok_list[idx.*].tok == Tok_enum.PERIOD){
                return field_arr(alloc,idx,tok_list,res_idx,ast);
            }
            else{ return res_idx; }
        }
        // if there is no ] at the end of expression
        else{ return ParserError.UnclosedBrackArr; }
    }
    
    return ParserError.NotMatch;
}


fn atom(alloc: Allocator, idx: *usize, tok_list: []const lex.Token,ast : *ArrayList(Node)) !u32 {
    
    switch(tok_list[idx.*].tok){
        .BOOL_LIT,.STRING_LIT,.CHAR_LIT,.INT_LIT,.FLOAT_LIT => {
            var lit = Node{.typ = @intToEnum(NodeType,@enumToInt(tok_list[idx.*].tok) - @enumToInt(Tok_enum.STRING_LIT) + @enumToInt(NodeType.STRING_LIT)),.children = ArrayList(u32).init(alloc)};
            lit.value = tok_list[idx.*].value.?;
            idx.* += 1;
            try ast.append(lit);
            return @truncate(u32, ast.items.len - 1);
        },
        .IDENTIFIER => {
             var id = Node{.typ = .ID ,.children =  ArrayList(u32).init(alloc), .value = tok_list[idx.*].value.? };
             errdefer id.children.deinit();

             try ast.append(id);
             const id_idx = @truncate(u32, ast.items.len - 1);
             
             idx.* += 1;
             const tok = tok_list[idx.*].tok;
             
             switch(tok){
                Tok_enum.L_PAREN => { // func call
                    idx.* += 1;
                    var func = Node{.typ = .FUNC_CALL ,.children = ArrayList(u32).init(alloc)};
                    errdefer func.children.deinit();

                    try ast.append(func);
                    const func_idx = @truncate(u32, ast.items.len - 1);
                    
                    const params = try func_params(alloc,idx,tok_list,ast);

                    if(tok_list[idx.*].tok == Tok_enum.R_PAREN){
                        idx.* += 1;
                        //return func;
                        try ast.items[func_idx].children.append(id_idx); // add id to ast and add it as function children
                        try ast.items[func_idx].children.append(params);
                        return func_idx;
                    }
                    
                    return ParserError.UnclosedParenFunc;
                },
                Tok_enum.PERIOD,Tok_enum.L_BRACK => {    
                    return field_arr(alloc, idx, tok_list, id_idx, ast);
                },
                else => {}
             }
            //return id since its nothing else
            return id_idx;
        },
        else => {
            // if its nothing other, check for expression in parenthesis   
            if(tok_list[idx.*].tok == Tok_enum.L_PAREN){
                idx.* += 1;
                const exp = try and_or(alloc,idx,tok_list,ast,null);
                // if it returns ParserError then its okay, since there should be expression here, if anything else then good too since we don't want errors
                if(tok_list[idx.*].tok == Tok_enum.R_PAREN){ idx.* += 1; return exp; }
                else{ return ParserError.UnclosedParenExpr; }
            }
            // if not, maybe its array literal, so do the same shit as func but for array
            if(tok_list[idx.*].tok == Tok_enum.L_CURLY){
                idx.* += 1;
                var arr_lit = Node{.typ = .ARR_LIT ,.children =  ArrayList(u32).init(alloc) };
                errdefer arr_lit.children.deinit();

                try ast.append(arr_lit);
                const arr_idx = @truncate(u32, ast.items.len - 1);
                
                const params = try func_params(alloc,idx,tok_list,ast);
                if(tok_list[idx.*].tok == Tok_enum.R_CURLY){
                    idx.* += 1;
                    try ast.items[arr_idx].children.append(params);
                    return arr_idx;
                }
            }
               
        }
    }
    std.debug.print("\n{s}\n",.{tok_list[idx.*]});
    return ParserError.NotMatch;
}

fn unary(alloc: Allocator, idx: *usize,tok_list: []const lex.Token, ast : *ArrayList(Node)) anyerror!u32 {
    const symbol : ?Tok_enum = if(tok_list[idx.*].tok == Tok_enum.NOT or tok_list[idx.*].tok == Tok_enum.SUB) tok_list[idx.*].tok else null;
    
    if(symbol == null){ // if no symbol at front, then it must be atom
        return atom(alloc,idx,tok_list,ast);
    }
    else{
        var un = Node{.typ = if(symbol == Tok_enum.NOT) .NOT else .NEG,.children = ArrayList(u32).init(alloc)};
        errdefer un.children.deinit();
        idx.* += 1;
        
        try ast.append(un);
        const tree_idx = @truncate(u32, ast.items.len - 1);
        
        try ast.items[tree_idx].children.append(try unary(alloc,idx,tok_list,ast));
        return tree_idx;
    }
    return ParserError.NotMatch;
}

fn factor(alloc: Allocator, idx: *usize, tok_list: []const lex.Token,ast : *ArrayList(Node), lh : ?u32) !u32 {
    var lhs : u32 = undefined; 
    if(lh == null) { lhs = try unary(alloc, idx, tok_list, ast); }
    else { lhs = lh.?; }
    
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
        } ,.children = ArrayList(u32).init(alloc)};
        errdefer tree.children.deinit();
        idx.* += 1;

        try ast.append(tree);
        const tree_idx = @truncate(u32, ast.items.len - 1);
        
        const rhs = try unary(alloc,idx,tok_list,ast);
        
        try ast.items[tree_idx].children.append(lhs);
        try ast.items[tree_idx].children.append(rhs);

        if(tok_list[idx.*].tok == Tok_enum.MUL or tok_list[idx.*].tok == Tok_enum.DIV or tok_list[idx.*].tok == Tok_enum.MOD) 
        { return factor(alloc,idx,tok_list,ast,tree_idx); }

        return tree_idx;
    }

    return ParserError.NotMatch;
}

fn term(alloc: Allocator, idx: *usize, tok_list: []const lex.Token,ast : *ArrayList(Node), lh : ?u32) !u32 {
    var lhs : u32 = undefined; 
    if(lh == null) { lhs = try factor(alloc, idx, tok_list, ast, null); }
    else { lhs = lh.?; }
    
    const next_tok : ?Tok_enum = if(tok_list[idx.*].tok == Tok_enum.ADD or tok_list[idx.*].tok == Tok_enum.SUB) tok_list[idx.*].tok else null;
    
    // no operator found after it
    if(next_tok == null){ return lhs; }
    else{
        
        var tree = Node{.typ = if(next_tok.? == Tok_enum.ADD) .ADD else .SUB ,.children = ArrayList(u32).init(alloc)};
        errdefer tree.children.deinit();
        idx.* += 1;

        try ast.append(tree);
        const tree_idx = @truncate(u32, ast.items.len - 1);

        const rhs = try factor(alloc,idx,tok_list,ast,null);

        try ast.items[tree_idx].children.append(lhs);
        try ast.items[tree_idx].children.append(rhs);

        if(tok_list[idx.*].tok == Tok_enum.ADD or tok_list[idx.*].tok == Tok_enum.SUB) { return term(alloc,idx,tok_list,ast,tree_idx); }

        return tree_idx;
    }
    return ParserError.NotMatch;
}


fn comparison(alloc: Allocator, idx: *usize, tok_list: []const lex.Token,ast : *ArrayList(Node), lh : ?u32) !u32 {
    var lhs : u32 = undefined; 
    if(lh == null) { lhs = try term(alloc, idx, tok_list, ast, null); }
    else { lhs = lh.?; }
    
    const next_tok : ?Tok_enum = if(tok_list[idx.*].tok == .BIGGER or tok_list[idx.*].tok == .SMALLER) tok_list[idx.*].tok else null;

    // no operator found after it
    if(next_tok == null){ return lhs; }
    else{
        //_ = ast.items[pan_idx].children.pop(); // remove lhs from children of parent
        var tree : Node = undefined;
        if(tok_list[idx.* + 1].tok == .EQU){ // its >= or <=
            tree = Node{.typ = if(next_tok.? == .BIGGER) .MORE_E else .LESS_E ,.children = ArrayList(u32).init(alloc)};
            idx.* += 2;
        }
        else{ // its > or < 
            tree = Node{.typ = if(next_tok.? == .BIGGER) .MORE else .LESS ,.children = ArrayList(u32).init(alloc)};
            idx.* += 1;
        }
        errdefer tree.children.deinit();

        try ast.append(tree);
        const tree_idx = @truncate(u32, ast.items.len - 1);
        
        const rhs = try term(alloc,idx,tok_list,ast,null);       

        try ast.items[tree_idx].children.append(lhs);
        try ast.items[tree_idx].children.append(rhs);

        if(tok_list[idx.*].tok == .BIGGER or tok_list[idx.*].tok == .SMALLER) { return comparison(alloc,idx,tok_list,ast,tree_idx); }
        
        return tree_idx;
    }
    return ParserError.NotMatch;
}

fn equality(alloc: Allocator, idx: *usize,tok_list: []const lex.Token, ast : *ArrayList(Node), lh : ?u32) !u32 {
    var lhs : u32 = undefined; 
    if(lh == null) { lhs = try comparison(alloc, idx, tok_list, ast, null); }
    else { lhs = lh.?; }
    
    const next_tok : ?Tok_enum = if(tok_list[idx.*].tok == .EQU or tok_list[idx.*].tok == Tok_enum.NOT) tok_list[idx.*].tok else null;

    // no operator found after it
    if(next_tok == null){ return lhs; }
    else{
        if(tok_list[idx.* + 1].tok == .EQU){
            
            // we initalize our logic node which is either == or != depends on the token and we add lhs and rhs as its two children nodes
            var tree = Node{.typ = if(next_tok.? == .EQU) .EQUAL else .N_EQUAL ,.children = ArrayList(u32).init(alloc)};
            errdefer tree.children.deinit();
            idx.* += 2;
            
            try ast.append(tree);
            const tree_idx = @truncate(u32, ast.items.len - 1);
        
            const rhs = try comparison(alloc,idx,tok_list,ast,null);

            try ast.items[tree_idx].children.append(lhs);
            try ast.items[tree_idx].children.append(rhs);

            if(tok_list[idx.*].tok == .EQU or tok_list[idx.*].tok == Tok_enum.NOT) { return equality(alloc,idx,tok_list,ast,tree_idx); }

            return tree_idx;
        }
        else{ return ParserError.IncorrectEqualOp; }
    }
    return ParserError.NotMatch;
} 

// XXX TODO use optional-if with next_tok for expression functions, would be better 

fn and_or(alloc: Allocator, idx: *usize, tok_list: []const lex.Token, ast : *ArrayList(Node), lh : ?u32) anyerror!u32 {
    var lhs : u32 = undefined; 
    if(lh == null) { lhs = try equality(alloc, idx, tok_list, ast, null); }
    else { lhs = lh.?; }
    
    const next_tok : ?Tok_enum = if(tok_list[idx.*].tok == Tok_enum.AND or tok_list[idx.*].tok == Tok_enum.OR) tok_list[idx.*].tok else null;
    
    // no operator found after it
    if(next_tok == null){ return lhs; }
    else{
        
        var tree = Node{.typ = if(next_tok.? == Tok_enum.AND) .AND else .OR ,.children = ArrayList(u32).init(alloc)};
        errdefer tree.children.deinit();
        idx.* += 1;

        try ast.append(tree);
        const tree_idx = @truncate(u32, ast.items.len - 1);
        
        const rhs = try equality(alloc, idx, tok_list, ast,null);
        
        try ast.items[tree_idx].children.append(lhs);
        try ast.items[tree_idx].children.append(rhs);

        
        if(tok_list[idx.*].tok == Tok_enum.AND or tok_list[idx.*].tok == Tok_enum.OR) { return and_or(alloc,idx,tok_list,ast,tree_idx); }

        // we check for operators after expression ( AND and OR keywords)
        // if operator is found, we know we gotta go further so we pass the expression as our left side and do everything again
        return tree_idx;
    }
    
    return ParserError.NotMatch;
}

fn expression(alloc: Allocator, idx: *usize,tok_list: []const lex.Token, ast : *ArrayList(Node), pan_idx: u32) !void {
    var exp = Node{.typ = .EXPR, .children = ArrayList(u32).init(alloc)};
    errdefer exp.children.deinit();

    try ast.append(exp); // Add exp node to array of nodes (add it on tree)
    const exp_idx = @truncate(u32, ast.items.len - 1);
    try ast.items[pan_idx].children.append(exp_idx); // Add exp as children to parent sitting at pan_idx
    const result = try and_or(alloc,idx,tok_list,ast,null);

    try ast.items[exp_idx].children.append(result);
}

fn statement(alloc: Allocator, idx: *usize,tok_list: []const lex.Token, ast : *ArrayList(Node), pan_idx: u32) anyerror!void {
    var stat = Node{.typ = .STATEMENT, .children = ArrayList(u32).init(alloc)};
    errdefer stat.children.deinit();

    switch(tok_list[idx.*].tok){
        Tok_enum.PRINT => {
            stat.typ = .PRINT;
            try ast.append(stat);
            const print_idx = @truncate(u32, ast.items.len - 1);
            idx.* += 1;
            try expression(alloc, idx, tok_list, ast, print_idx);
            try ast.items[pan_idx].children.append(print_idx);
        },
        Tok_enum.INPUT => {
            stat.typ = .INPUT;
            try ast.append(stat);
            const input_idx = @truncate(u32, ast.items.len - 1);
            idx.* += 1;
            if(tok_list[idx.*].tok == Tok_enum.IDENTIFIER){
                var id = Node{.typ = .ID ,.children =  ArrayList(u32).init(alloc), .value = tok_list[idx.*].value.? };
                errdefer id.children.deinit();
                try ast.append(id);

                try ast.items[input_idx].children.append(@truncate(u32, ast.items.len - 1));
                
                try ast.items[pan_idx].children.append(input_idx);
                idx.* += 1;
            }
            else{ return ParserError.NoIDGiven; }
        },
        // Variable declaration
        Tok_enum.PIWO => {
            stat.typ = .VAR_DECL;
            try ast.append(stat);
            const decl_idx = @truncate(u32, ast.items.len - 1);
            idx.* += 1;
            var is_kurwa : bool = false;
            const is_type = @enumToInt(tok_list[idx.*].tok) >= @enumToInt(Tok_enum.STRING_TYPE)
                        and @enumToInt(tok_list[idx.*].tok) <= @enumToInt(Tok_enum.KURWA_TYPE);

            if(tok_list[idx.*].tok == Tok_enum.IDENTIFIER or is_type){
                is_kurwa = tok_list[idx.*].tok == Tok_enum.KURWA_TYPE;
                var typ_node : Node = undefined;
                errdefer typ_node.children.deinit();
                if(is_type) {
                    const t = @intToEnum(NodeType,@enumToInt(tok_list[idx.*].tok) - @enumToInt(Tok_enum.STRING_TYPE) + @enumToInt(NodeType.STRING));
                    typ_node = Node{.typ = t, .children = ArrayList(u32).init(alloc)};
                }
                else{
                    typ_node = Node{.typ = .ID, .children = ArrayList(u32).init(alloc), .value = tok_list[idx.*].value.?};            
                }
                try ast.append(typ_node);
                try ast.items[decl_idx].children.append(@truncate(u32, ast.items.len - 1));
                
            }else { return ParserError.IncorrectType; }
            idx.* += 1;

            if(tok_list[idx.*].tok == Tok_enum.IDENTIFIER){
                var id = Node{.typ = .ID, .children = ArrayList(u32).init(alloc), .value = tok_list[idx.*].value.?};
                errdefer id.children.deinit();
                try ast.append(id);
                try ast.items[decl_idx].children.append(@truncate(u32, ast.items.len - 1));
            }
            else{ return ParserError.NoIDGiven;}
            idx.* += 1;

            if(tok_list[idx.*].tok == Tok_enum.EQU or is_kurwa) {
                    if(is_kurwa and tok_list[idx.*].tok != Tok_enum.EQU) return ParserError.NoAssignWhenNeeded; 
                    idx.* += 1;
                    try expression(alloc, idx, tok_list, ast, decl_idx);
            }
            
            try ast.items[pan_idx].children.append(decl_idx);
        },
        // Constant declaration
        Tok_enum.WINO => {
                    stat.typ = .CONST_DECL;
                    try ast.append(stat);
                    const decl_idx = @truncate(u32, ast.items.len - 1);
                    idx.* += 1;
                    
                    const is_type = @enumToInt(tok_list[idx.*].tok) >= @enumToInt(Tok_enum.STRING_TYPE)
                                and @enumToInt(tok_list[idx.*].tok) <= @enumToInt(Tok_enum.KURWA_TYPE);
        
                    if(tok_list[idx.*].tok == Tok_enum.IDENTIFIER or is_type){
                        var typ_node : Node = undefined;
                        errdefer typ_node.children.deinit();
                        if(is_type) {
                            const t = @intToEnum(NodeType,@enumToInt(tok_list[idx.*].tok) - @enumToInt(Tok_enum.STRING_TYPE) + @enumToInt(NodeType.STRING));
                            typ_node = Node{.typ = t, .children = ArrayList(u32).init(alloc)};
                        }
                        else{
                            typ_node = Node{.typ = .ID, .children = ArrayList(u32).init(alloc), .value = tok_list[idx.*].value.?};            
                        }
                        try ast.append(typ_node);
                        try ast.items[decl_idx].children.append(@truncate(u32, ast.items.len - 1));
                        
                    }else { return ParserError.IncorrectType; }
                    idx.* += 1;
        
                    if(tok_list[idx.*].tok == Tok_enum.IDENTIFIER){
                        var id = Node{.typ = .ID, .children = ArrayList(u32).init(alloc), .value = tok_list[idx.*].value.?};
                        errdefer id.children.deinit();
                        try ast.append(id);
                        try ast.items[decl_idx].children.append(@truncate(u32, ast.items.len - 1));
                    }
                    else{ return ParserError.NoIDGiven;}
                    idx.* += 1;
        
                    if(tok_list[idx.*].tok == Tok_enum.EQU) { 
                            idx.* += 1;
                            try expression(alloc, idx, tok_list, ast, decl_idx);
                    }
                    else { return ParserError.NoAssignWhenNeeded; }
                    
                    try ast.items[pan_idx].children.append(decl_idx);
                },
        // Array declaration
        Tok_enum.KUFEL => { 
                    stat.typ = .ARR_DECL;
                    try ast.append(stat);
                    const decl_idx = @truncate(u32, ast.items.len - 1);
                    idx.* += 1;
                    
                    var is_kurwa: bool = false;
                    const is_type = @enumToInt(tok_list[idx.*].tok) >= @enumToInt(Tok_enum.STRING_TYPE)
                                and @enumToInt(tok_list[idx.*].tok) <= @enumToInt(Tok_enum.KURWA_TYPE);
                    
                    if(tok_list[idx.*].tok == Tok_enum.IDENTIFIER or is_type){
                        is_kurwa = tok_list[idx.*].tok == Tok_enum.KURWA_TYPE;
                        var typ_node : Node = undefined;
                        errdefer typ_node.children.deinit();
                        if(is_type) {
                            const t = @intToEnum(NodeType,@enumToInt(tok_list[idx.*].tok) - @enumToInt(Tok_enum.STRING_TYPE) + @enumToInt(NodeType.STRING));
                            typ_node = Node{.typ = t, .children = ArrayList(u32).init(alloc)};
                        }
                        else{
                            typ_node = Node{.typ = .ID, .children = ArrayList(u32).init(alloc), .value = tok_list[idx.*].value.?};            
                        }
                        try ast.append(typ_node);
                        try ast.items[decl_idx].children.append(@truncate(u32, ast.items.len - 1));
                        
                    }else { return ParserError.IncorrectType; }
                    idx.* += 1;

                    if(tok_list[idx.*].tok == Tok_enum.IDENTIFIER){
                        var id = Node{.typ = .ID, .children = ArrayList(u32).init(alloc), .value = tok_list[idx.*].value.?};
                        errdefer id.children.deinit();
                        try ast.append(id);
                        try ast.items[decl_idx].children.append(@truncate(u32, ast.items.len - 1));
                    }
                    else{ return ParserError.NoIDGiven;}
                    idx.* += 1;
                
                    if(tok_list[idx.*].tok != Tok_enum.L_BRACK) return ParserError.NoArrBrack;

                    var params = Node{.typ = .PARAMETERS, .children = ArrayList(u32).init(alloc)};
                    errdefer params.children.deinit();
                    try ast.append(params);
                    const params_idx =  @truncate(u32, ast.items.len - 1);

                    while(tok_list[idx.*].tok == Tok_enum.L_BRACK){
                        idx.* += 1;
                        if(expression(alloc, idx, tok_list, ast, params_idx)) {} else |err| {
                            if(err != ParserError.NotMatch) return err;
                        }
                        
                        if(tok_list[idx.*].tok == Tok_enum.R_BRACK){ idx.* += 1; }
                        else{ return ParserError.UnclosedBrackArr; }
                    }
                    
                    try ast.items[decl_idx].children.append(params_idx);

                    if(tok_list[idx.*].tok == Tok_enum.EQU or is_kurwa) {
                            if(is_kurwa and tok_list[idx.*].tok != Tok_enum.EQU) return ParserError.NoAssignWhenNeeded; 
                            idx.* += 1;
                            try expression(alloc, idx, tok_list, ast, decl_idx);
                    }
                    
                    try ast.items[pan_idx].children.append(decl_idx);
                },
        Tok_enum.BECZKA => {
                            stat.typ = .STRUCT_DECL;
                            try ast.append(stat);
                            const decl_idx = @truncate(u32, ast.items.len - 1);
                            idx.* += 1;
                            
                            if(tok_list[idx.*].tok == Tok_enum.IDENTIFIER){
                                var id = Node{.typ = .ID, .children = ArrayList(u32).init(alloc), .value = tok_list[idx.*].value.?};
                                errdefer id.children.deinit();
                                try ast.append(id);
                                try ast.items[decl_idx].children.append(@truncate(u32, ast.items.len - 1));
                            }
                            else{ return ParserError.NoIDGiven;}
                            idx.* += 1;
                
                            if(tok_list[idx.*].tok == Tok_enum.EQU) { idx.* += 1; }
                            else { return ParserError.NoAssignWhenNeeded; }

                            if(tok_list[idx.*].tok == Tok_enum.L_CURLY){ idx.* += 1; }
                            else { return ParserError.NoStructBrack; }
if(tok_list[idx.*].tok == Tok_enum.NEWLINE){ idx.* += 1; }

                            // TODO Make it a function, so i can reuse it in function declaration
if(tok_list[idx.*].tok == Tok_enum.INDENT){ idx.* += 1; }
if(tok_list[idx.*].tok == Tok_enum.NEWLINE){ idx.* += 1; }
                            var fields = Node{.typ = .PARAMETERS, .children = ArrayList(u32).init(alloc)};
                            errdefer fields.children.deinit();
                            try ast.append(fields);
                            const fields_idx = @truncate(u32, ast.items.len - 1);

                            try ast.items[decl_idx].children.append(fields_idx);
                            
                            while(tok_list[idx.*].tok == Tok_enum.IDENTIFIER) {
                                // making field node
                                var field = Node{.typ = .FIELD, .children = ArrayList(u32).init(alloc)};
                                errdefer field.children.deinit();
                                try ast.append(field);
                                
                                const field_idx = @truncate(u32, ast.items.len - 1);
                                try ast.items[fields_idx].children.append(field_idx);

                                // adding ID from the field
                                var id = Node{.typ = .ID, .children = ArrayList(u32).init(alloc), .value = tok_list[idx.*].value.?};
                                errdefer id.children.deinit();
                                try ast.append(id);
                                try ast.items[field_idx].children.append(@truncate(u32, ast.items.len - 1));
                                idx.* += 1;
                                
                                // checking for colon between ID and type
                                if(tok_list[idx.*].tok == Tok_enum.COLON){ idx.* += 1; }
                                else { return expectToken(Tok_enum.COLON, tok_list, idx.*); }

                                // checking and adding
                                const is_type = @enumToInt(tok_list[idx.*].tok) >= @enumToInt(Tok_enum.STRING_TYPE)
                                            and @enumToInt(tok_list[idx.*].tok) <= @enumToInt(Tok_enum.KURWA_TYPE);
                                if(tok_list[idx.*].tok == Tok_enum.KURWA_TYPE){ return ParserError.KurwaNotAllowed; }
                                if(tok_list[idx.*].tok == Tok_enum.IDENTIFIER or is_type){
                                var typ_node : Node = undefined;
                                errdefer typ_node.children.deinit();

                                if(is_type) {
                                    const t = @intToEnum(NodeType,@enumToInt(tok_list[idx.*].tok) - @enumToInt(Tok_enum.STRING_TYPE) + @enumToInt(NodeType.STRING));
                                    typ_node = Node{.typ = t, .children = ArrayList(u32).init(alloc)};
                                }
                                else{
                                    typ_node = Node{.typ = .ID, .children = ArrayList(u32).init(alloc), .value = tok_list[idx.*].value.?};
                                }
                                try ast.append(typ_node);
                                try ast.items[field_idx].children.append(@truncate(u32, ast.items.len - 1));
                                
                                }else { return ParserError.IncorrectType; }

                                idx.* += 1;
                                
                                // ignore newlines, check for comma for next field
if(tok_list[idx.*].tok == Tok_enum.NEWLINE){ idx.* += 1; }
                                if(tok_list[idx.*].tok == Tok_enum.COMMA){ idx.* += 1; }                                
if(tok_list[idx.*].tok == Tok_enum.DEDENT){ idx.* += 1; }
if(tok_list[idx.*].tok == Tok_enum.NEWLINE){ idx.* += 1; }
                          }

if(tok_list[idx.*].tok == Tok_enum.DEDENT){ idx.* += 1; }                        
                            if(tok_list[idx.*].tok == Tok_enum.R_CURLY){ idx.* += 1; }
                            else { return expectToken(Tok_enum.R_CURLY, tok_list, idx.*); }
                            // end
                            try ast.items[pan_idx].children.append(decl_idx);
            },
        
        // Assign statement
        Tok_enum.IDENTIFIER => {
            stat.typ = .ASSIGN;
            try ast.append(stat);
            const assign_idx = @truncate(u32, ast.items.len - 1);

            var id = Node{.typ = .ID, .children = ArrayList(u32).init(alloc), .value = tok_list[idx.*].value.?};
            errdefer id.children.deinit();

            try ast.append(id);
            try ast.items[assign_idx].children.append(@truncate(u32, ast.items.len - 1));
            idx.* += 1;
            
            if(tok_list[idx.*].tok == Tok_enum.EQU){
                idx.* += 1;
                try expression(alloc, idx, tok_list, ast, assign_idx);
            }else { return expectToken(Tok_enum.EQU, tok_list, idx.*); }
            
            try ast.items[pan_idx].children.append(assign_idx);
        },
        // While loop statement
        Tok_enum.WHILE => {
            stat.typ = .WHILE;
            try ast.append(stat);
            const block_idx = @truncate(u32, ast.items.len - 1);
            idx.* += 1;
            try expression(alloc, idx, tok_list, ast, block_idx);

            if(tok_list[idx.*].tok == Tok_enum.COLON){ idx.* += 1; }
            else { return expectToken(Tok_enum.COLON, tok_list, idx.*); }

            // one line statement
            if(tok_list[idx.*].tok != Tok_enum.NEWLINE){
                try statement(alloc, idx, tok_list, ast, block_idx);
                if(tok_list[idx.*].tok == Tok_enum.NEWLINE){ idx.* += 1; }
                try ast.items[pan_idx].children.append(block_idx);
                return;
            }
            // -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
            if(tok_list[idx.*].tok == Tok_enum.NEWLINE){ idx.* += 1; }
            else { return expectToken(Tok_enum.NEWLINE, tok_list, idx.*); }

            if(tok_list[idx.*].tok == Tok_enum.INDENT){ idx.* += 1; }
            else { return ParserError.NoIndentForBlock; }

            while(tok_list[idx.*].tok != Tok_enum.DEDENT){
                try statement(alloc, idx, tok_list, ast, block_idx);
                if(tok_list[idx.*].tok == Tok_enum.NEWLINE){ idx.* += 1; }
            }
            idx.* += 1;

            try ast.items[pan_idx].children.append(block_idx);
        },
        Tok_enum.IF => {
            stat.typ = .IF;
            try ast.append(stat);
            const block_idx = @truncate(u32, ast.items.len - 1);
            idx.* += 1;
            try expression(alloc, idx, tok_list, ast, block_idx);

            if(tok_list[idx.*].tok == Tok_enum.COLON){ idx.* += 1; }
            else { return expectToken(Tok_enum.COLON, tok_list, idx.*); }

            // one line statement
            if(tok_list[idx.*].tok != Tok_enum.NEWLINE){
                try statement(alloc, idx, tok_list, ast, block_idx);
                if(tok_list[idx.*].tok == Tok_enum.NEWLINE){ idx.* += 1; }
                try ast.items[pan_idx].children.append(block_idx);
                return;
            }
            // -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
            if(tok_list[idx.*].tok == Tok_enum.NEWLINE){ idx.* += 1; }
            else { return expectToken(Tok_enum.NEWLINE, tok_list, idx.*); }

            if(tok_list[idx.*].tok == Tok_enum.INDENT){ idx.* += 1; }
            else { return ParserError.NoIndentForBlock; }

            while(tok_list[idx.*].tok != Tok_enum.DEDENT){
                try statement(alloc, idx, tok_list, ast, block_idx);
                if(tok_list[idx.*].tok == Tok_enum.NEWLINE){ idx.* += 1; }
            }
            idx.* += 1;

            try ast.items[pan_idx].children.append(block_idx);
        },
        Tok_enum.FOREACH => {
            stat.typ = .FOREACH;
            try ast.append(stat);
            const block_idx = @truncate(u32, ast.items.len - 1);
            idx.* += 1;

            //     vvv
            // dla arr -> el:
            if(tok_list[idx.*].tok == Tok_enum.IDENTIFIER){}
            else { return expectToken(Tok_enum.IDENTIFIER, tok_list, idx.*); }
            var arr = Node{.typ = .ID, .children = ArrayList(u32).init(alloc), .value = tok_list[idx.*].value.?};
            errdefer arr.children.deinit();

            try ast.append(arr);
            try ast.items[block_idx].children.append(@truncate(u32, ast.items.len - 1));
            idx.* += 1;

            //         vv
            // dla arr -> el:
            if(tok_list[idx.*].tok == Tok_enum.ARROW){ idx.* += 1; }
            else { return expectToken(Tok_enum.ARROW, tok_list, idx.*); }

            //            vv
            // dla arr -> el:
            if(tok_list[idx.*].tok == Tok_enum.IDENTIFIER){}
            else { return expectToken(Tok_enum.IDENTIFIER, tok_list, idx.*); }
            var el = Node{.typ = .ID, .children = ArrayList(u32).init(alloc), .value = tok_list[idx.*].value.?};
            errdefer el.children.deinit();

            try ast.append(el);
            try ast.items[block_idx].children.append(@truncate(u32, ast.items.len - 1));
            idx.* += 1;
            
            if(tok_list[idx.*].tok == Tok_enum.COLON){ idx.* += 1; }
            else { return expectToken(Tok_enum.COLON, tok_list, idx.*); }
            std.debug.print("HERER\n\n",.{});
            // one line statement
            if(tok_list[idx.*].tok != Tok_enum.NEWLINE){
                try statement(alloc, idx, tok_list, ast, block_idx);
                if(tok_list[idx.*].tok == Tok_enum.NEWLINE){ idx.* += 1; }
                try ast.items[pan_idx].children.append(block_idx);
                return;
            }
            // -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
            if(tok_list[idx.*].tok == Tok_enum.NEWLINE){ idx.* += 1; }
            else { return expectToken(Tok_enum.NEWLINE, tok_list, idx.*); }

            if(tok_list[idx.*].tok == Tok_enum.INDENT){ idx.* += 1; }
            else { return ParserError.NoIndentForBlock; }

            while(tok_list[idx.*].tok != Tok_enum.DEDENT){
                try statement(alloc, idx, tok_list, ast, block_idx);
                if(tok_list[idx.*].tok == Tok_enum.NEWLINE){ idx.* += 1; }
            }
            idx.* += 1;

            try ast.items[pan_idx].children.append(block_idx);
        },

        else => {
            return ParserError.NotMatch;
        } 
    }
}

fn parse(tok_list: []const lex.Token, alloc: Allocator) !ArrayList(Node) {
    // Idk i feel like its funny to estimate how many nodes there might be in the Ast 
    var LINES : usize = 1;
    for(tok_list) |t| { if(t.tok == Tok_enum.NEWLINE) LINES += 1;}
    const AST_CAPACITY = LINES * AVG_NODE_PER_LINE;
    
    var Ast = try ArrayList(Node).initCapacity(alloc,AST_CAPACITY);
    
    try Ast.append(Node{.typ = .PROGRAM, .children = ArrayList(u32).init(alloc)});
    
    var idx : usize = 0;
    
    errdefer {
        freeAST(tok_list,Ast,alloc);
        // TODO in future use token to print out where the error is and only then free it
    }
    while(true){
        try statement(alloc, &idx, tok_list, &Ast, 0);
        if(tok_list[idx].tok == Tok_enum.NEWLINE){ idx += 1; }
        if(tok_list[idx].tok == Tok_enum.EOF){ break; }

        
        // else{ return ParserError.NotMatch; } // statement doesn't end with newline so something is wrong
    }

    return Ast;
}


pub fn freeAST(tokens: []const lex.Token,res: ArrayList(Node),alloc: Allocator) void {
    lex.freeTokenValues(tokens,alloc,0);
    alloc.free(tokens);
    freeNodesValues(res);     // free node (the AST) all its children and their values
    res.deinit();
}

fn freeNodesValues(nodes: ArrayList(Node)) void {
    var i : u32 = 0;
    while( i < nodes.items.len ) : (i += 1) {
        nodes.items[i].children.deinit();
    }
}

pub fn printNodes(ast: ArrayList(Node),idx : u32,deep: u8) std.os.WriteError!void {
    const stdout = std.io.getStdOut().writer();
    if(deep == 0) try stdout.print("\n", .{});
    
    var i : u8 = deep;
    while(i != 0) : ( i -= 1){
        try stdout.print("    ", .{});
    }
    
    try stdout.print("- {s} {s}\n", .{@tagName(ast.items[idx].typ), if (ast.items[idx].value != null) ast.items[idx].value.? else ""});
    
    for(ast.items[idx].children.items) |n| {
        try printNodes(ast,n,deep + 1);      
    }
}

const eql = std.mem.eql;
const expect = std.testing.expect;
const test_alloc = std.testing.allocator;


// if(true) return error.SkipZigTest;
test "par expression" {
    if(true) return error.SkipZigTest;

    var tokens = try lex.tokenize("{abc, \"string\", 560}", test_alloc);
    var res    = try parse(tokens, test_alloc);
    defer    freeAST(tokens,res,test_alloc);
    try printNodes(res,0,0);
}

test "par just testing" {
    // if(true) return error.SkipZigTest;
    const source = \\
    \\dla tablica -> element: wypisz element
    \\jezeli tak: wypisz "true"
    \\dopóki nie: wypisz "false"
    \\piwo int index = 10
    \\wino string some_const = "im a const"
    \\kufel char not_a_string_lol[] = {'\n','\t','\0'}
    ;
    
    var tokens = try lex.tokenize(source, test_alloc);
    var res    = try parse(tokens, test_alloc);
    defer    freeAST(tokens,res,test_alloc);

    try printNodes(res,0,0);
}

test "par unclosed parenthesis for function" {
    if(true) return error.SkipZigTest;
    var tokens = try lex.tokenize("foo(69", test_alloc);
    if(parse(tokens, test_alloc)) |res|{
        freeAST(tokens,res,test_alloc);
        unreachable;
    }else |err| {
        try expect(err == ParserError.UnclosedParenFunc);
    }
}
