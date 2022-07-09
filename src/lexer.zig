const std = @import("std");
const ArrayList = std.ArrayList;

pub const Tok_enum = enum(u8) {
    UNKNOWN,
    NEWLINE,
    // Literals
    STRING_LIT,
    INT_LIT,
    CHAR_LIT,
    FLOAT_LIT,
    BOOL_LIT,
    // Types (gets compared from here)
    STRING_TYPE,
    INT_TYPE,
    CHAR_TYPE,
    FLOAT_TYPE,
    BOOL_TYPE,
    KURWA_TYPE,
    // Keywords
    PIWO,
    KUFEL,
    WINO,
    // Operators
    EQUAL,
    ADD,
    SUB,
    MUL,
    DIV,
    IF,
    IF_PL,
    ELSE,
    FOREACH,
    WHILE,
    WHILE_PL,
    ARROW,
    FUNC,
    RETURN,
    RETURN_PL,
    INPUT,
    PRINT,
    CONT,
    BREAK,
    
    IDENTIFIER,
    INDENT,
    DEDENT
};

const cmp_words = [_][]const u8{
    "string",
    "int",
    "char",
    "float",
    "bool",
    "kurwa",
    "piwo",
    "kufel",
    "wino",
    "=",
    "+",
    "-",
    "*",
    "/",
    "jezeli",
    "jeżeli",
    "inaczej",
    "dla",
    "dopoki",
    "dopóki",
    "->",
    "funkcja",
    "zwroc",
    "zwróć",
    "podaj",
    "wypisz",
    "dalej",
    "przerwij"
};

pub const Token = struct{tok : Tok_enum, value : ?[]const u8 = null};
const SPACE_INDENT = 4;

const LexerError = error {
    InvalidIndentation,
};

fn readFileToString(path: []const u8, allocator: std.mem.Allocator) ![]u8 {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    const expected_max_size = 2_000_000;
    return try file.reader().readAllAlloc(allocator, expected_max_size);
}


pub fn tokenize(buffer: []const u8, alloc: std.mem.Allocator) ![]Token {
    var token_list = ArrayList(Token).init(alloc);
    defer token_list.deinit();

    //TODO Check for comments, maybe make function skip_comments()

    var global_indent : u32 = 0;
    var it = std.mem.tokenize(u8, buffer, "\n");
    var line: []const u8 = undefined;
    
    while(true){
        line = it.next() orelse break;
 // - Check for indentation - 
        var line_start  : u32 = 0;
        var curr_indent : u32 = 0;
        var space_count : u8  = 0;
        
        while(line_start < line.len){
               if(line[line_start] == '\t'){
                    curr_indent += 1;
                    line_start  += 1;
               }
               else if(line[line_start] == ' '){
                    space_count += 1;
                    line_start  += 1;
                    if(space_count == SPACE_INDENT){ curr_indent += 1; space_count -= SPACE_INDENT;}
               }
               else{ break; }
        }
        // if line has nothing other than indentation, just ignore it lol
        if(line_start == line.len){ continue; }
        
        if(line_start >= line.len) return token_list.toOwnedSlice();
        var indent_diff : i32 = @intCast(i32,curr_indent) - @intCast(i32,global_indent);
        
        if(indent_diff >= 2){ return LexerError.InvalidIndentation; }// having invalid scope, because like bruh
        
        else if(indent_diff < 0){ // means we are going down the scope <<
            while(indent_diff < 0) : (indent_diff += 1) {
                try token_list.append(Token{.tok = .DEDENT});
            }
            global_indent = curr_indent;
        }
        else if(curr_indent != 0 and global_indent != curr_indent){ try token_list.append(Token{.tok = .INDENT}); global_indent += 1; }    
// -----------------------------------------------------------------------------------------------------

// Check for keywords and stuff separated by space
        var word_it = std.mem.tokenize(u8, line, " \t");
        var word: []const u8 = undefined;
    outer: while (true) {
        word = word_it.next() orelse break;
        for(cmp_words) |test_word,i| {
            if(eql(u8,test_word,word)){
                const tok = @intToEnum(Tok_enum,i + @enumToInt(Tok_enum.STRING_TYPE));
                try token_list.append(Token{
                .tok = switch(tok){ // if its a polish token, return an enum one place behind him which is the same one but not polish
                            .IF_PL,.RETURN_PL,.WHILE_PL => @intToEnum(Tok_enum,i + @enumToInt(Tok_enum.STRING_TYPE) - 1),
                            else => tok 
                       }
                });
                continue :outer;
            }
        }

        // TODO Check for operators and identifiers
        try token_list.append(Token{.tok = .IDENTIFIER, .value = word});

        }
        try token_list.append(Token{.tok = .NEWLINE});        
// --------------------------------------------------------------------------------------------------------------------
    }

    if(global_indent != 0){ // Recover from any scope left over at the end of the file
        while(global_indent > 0) : (global_indent -= 1) {
            try token_list.append(Token{.tok = .DEDENT});
        }
    }
    return token_list.toOwnedSlice();
}

const eql = std.mem.eql;
const expect = std.testing.expect;
const test_alloc = std.testing.allocator;

// because std.mem.eql and std.meta.eql has problems i made quick function to manually test slices of Tokens
// TODO maybe there is a better solution
fn testTokens(tokens: []const Token, res:[]Token) !void {
    for (tokens) |t,i| {
        try expect(t.tok == res[i].tok);
        if(t.value == null){ try expect(res[i].value == null); } // can have no value, so null
        else               { try expect(eql(u8, t.value.?, res[i].value.?)); } // or be a string so we compare the strings
    }
}


test "basic variable declaration" {
    const res = try tokenize("piwo int abcd = 10", test_alloc);
    defer test_alloc.free(res); // TODO Change the unknowns to proper tokens
    errdefer std.debug.print("\n==========================\n!!!RESULT: {s}\n==========================\n", .{res});
    const test1 = [_]Token{ 
            .{.tok = .PIWO},
            .{.tok = .INT_TYPE}, 
            .{.tok = .IDENTIFIER, .value = "abcd"}, 
            .{.tok = .EQUAL}, 
            .{.tok = .IDENTIFIER, .value = "10"},
            .{.tok = .NEWLINE},
    };
    try testTokens(test1[0..],res);
}

test "basic variable assign" {
    const res = try tokenize("abcd = 69.420", test_alloc);
    defer test_alloc.free(res); // TODO Change the identifiers to proper tokens
    errdefer std.debug.print("\n==========================\n!!!RESULT: {s}\n==========================\n", .{res});

    const test1 = [_]Token{
            .{.tok = .IDENTIFIER, .value = "abcd"},
            .{.tok = .EQUAL},
            .{.tok = .IDENTIFIER, .value = "69.420"},
            .{.tok = .NEWLINE},
    };
    
    try testTokens(test1[0..],res);
}


test "basic indentation" {
    const res = try tokenize("\tpiwo", test_alloc);
    defer test_alloc.free(res); 
    errdefer std.debug.print("\n==========================\n!!!RESULT: {s}\n==========================\n", .{res});

    const test1 = [_]Token{
            .{.tok = .INDENT},
            .{.tok = .PIWO},
            .{.tok = .NEWLINE},
    };
    
    try testTokens(test1[0..],res);
}

test "recovering from indentation" {
    const source = \\piwo
                   \\    piwo
                   \\        piwo
                   //\\piwo
                   ;
    const res = try tokenize(source, test_alloc);
    defer test_alloc.free(res);
    errdefer std.debug.print("\n==========================\n!!!RESULT: {s}\n==========================\n", .{res});

    const test1 = [_]Token{
            .{.tok = .PIWO},.{.tok = .NEWLINE},
            .{.tok = .INDENT},
            .{.tok = .PIWO},.{.tok = .NEWLINE},
            .{.tok = .INDENT},
            .{.tok = .PIWO},.{.tok = .NEWLINE},
            .{.tok = .DEDENT},.{.tok = .DEDENT},
            //.{.tok = .PIWO},.{.tok = .NEWLINE},
    };
    
    try testTokens(test1[0..],res);
}

test "leaving line empty with indentation" { 
    const source = \\piwo
                   \\    piwo
                   \\    
                   \\    piwo
                   ;
    const res = try tokenize(source, test_alloc);
    defer test_alloc.free(res);
    errdefer std.debug.print("\n==========================\n!!!RESULT: {s}\n==========================\n", .{res});

    const test1 = [_]Token{
            .{.tok = .PIWO},.{.tok = .NEWLINE},
            .{.tok = .INDENT},
            .{.tok = .PIWO},.{.tok = .NEWLINE},
            .{.tok = .PIWO},.{.tok = .NEWLINE},
            .{.tok = .DEDENT},
    };
    
    try testTokens(test1[0..],res);
}

// FIXME maybe find a better way to know if the token is polish, maybe translate it to normal token in future
test "unicode polish letters working - 1" {
    const res = try tokenize("jeżeli", test_alloc);
    defer test_alloc.free(res); 
    errdefer std.debug.print("\n==========================\n!!!RESULT: {s}\n==========================\n", .{res});

    const test1 = [_]Token{
            .{.tok = .IF},
            .{.tok = .NEWLINE},
    };
    
    try testTokens(test1[0..],res);
}

test "unicode polish letters working - 2" {
    const res = try tokenize("dopóki", test_alloc);
    defer test_alloc.free(res); 
    errdefer std.debug.print("\n==========================\n!!!RESULT: {s}\n==========================\n", .{res});

    const test1 = [_]Token{
            .{.tok = .WHILE},
            .{.tok = .NEWLINE},
    };
    
    try testTokens(test1[0..],res);
}
