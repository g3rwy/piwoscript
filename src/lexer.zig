const std = @import("std");
const ArrayList = std.ArrayList;
const readFileToString = @import("utils.zig").readFileToString;

pub const Tok_enum = enum(u8) {
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
    // other keywords
    IF,
    IF_PL,
    ELSE,
    FOREACH,
    WHILE,
    WHILE_PL,
    FUNC,
    RETURN,
    RETURN_PL,
    INPUT,
    PRINT,
    CONT,
    BREAK,
    OR,
    AND,
    TAK,NIE, // Bool literals for true and false
    
    IDENTIFIER,
    INDENT,
    DEDENT,

    L_PAREN, // (
    R_PAREN, // )

    L_BRACK, // [
    R_BRACK, // ]

    L_CURLY, // {
    R_CURLY, // }
    // Operators
    EQUAL,
    ADD,
    SUB,
    MUL,
    DIV,
    NEG,
    COLON,
    COMMA,
    PERIOD,
    MODULO,
    
    ARROW,
};
// TODO albo, oraz operators

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
"jezeli",
"jeżeli",
"inaczej",
"dla",
"dopoki",
"dopóki",
"funkcja",
"zwroc",
"zwróć",
"podaj",
"wypisz",
"dalej",
"przerwij",
"albo",
"oraz",
"tak","nie"
};

const one_char_ops = [_]u8{
    '(',')','[',']','{','}','=','+','-','*','/','!',':',',','.','%'
};

pub const Token = struct{tok : Tok_enum, value : ?[]const u8 = null};
const SPACE_INDENT = 4;

const LexerError = error {
    InvalidIndentation,
    UnclosedComment,
    UnclosedString,
    IncorrectSlashChar
};

const TokenizingState = enum{
    NORMAL,  
    COMMENT,
    STRING,
};

pub fn tokenizeFile(name: []const u8,alloc: std.mem.Allocator) ![]Token {
    const buffer = try readFileToString(name, alloc);
    defer alloc.free(buffer);
    return tokenize(buffer,alloc);
}

fn checkIfKeyword(token_list: *ArrayList(Token), word: []const u8, alloc: std.mem.Allocator) !bool {
        var found_keyword : bool = false;
        for(cmp_words) |test_word,i| {
                if(eql(u8,test_word,word)){
                    const tok = @intToEnum(Tok_enum , i + @enumToInt(Tok_enum.STRING_TYPE));

                    if(tok == .TAK or tok == .NIE){ // Quick and dirty solution for boolean literals
                        try token_list.*.append(Token{.tok = .BOOL_LIT, .value = try alloc.dupe(u8,if(tok == .TAK) "t" else "n")});
                        found_keyword = true;
                        break;
                    }
                    
                    try token_list.*.append(Token{
                    .tok = switch(tok){ // if its a polish token, return an enum one place behind him which is the same one but not polish
                                .IF_PL,.RETURN_PL,.WHILE_PL => @intToEnum(Tok_enum , i + @enumToInt(Tok_enum.STRING_TYPE) - 1),
                                else => tok
                           }
                    });
                    found_keyword = true;
                    break;
                }
        }
        return found_keyword;
}

pub fn printTokens(tokens: []Token) !void {
    const stdout = std.io.getStdOut().writer();
    for(tokens) |tok| {
        if(tok.tok == .NEWLINE){
            try stdout.print("|\n",.{});
        } else {
            if(tok.value == null){
                try stdout.print(" {s} ", .{@tagName(tok.tok)});
            } else {
                try stdout.print(" ({s},{s}) ", .{@tagName(tok.tok), tok.value});                
            } 
        }        
    }
}

pub fn tokenize(buffer: []const u8, alloc: std.mem.Allocator) ![]Token {
    var token_list = ArrayList(Token).init(alloc);
    defer token_list.deinit();
    errdefer for(token_list.items) |*t| { if(t.value != null) alloc.free(t.value.?); };
    
    var curr_state = TokenizingState.NORMAL;
    var global_indent : u32 = 0;

    var it = std.mem.tokenize(u8, buffer, "\n\r");
    var line: []const u8 = undefined;
    
    while(true){
 // - Check for indentation - 
        line = it.next() orelse break;
        var line_start  : u32 = 0;
        var curr_indent : u32 = 0;
        var space_count : u8  = 0;

        // skip out comments if we are still in comment mode
        if(curr_state == .COMMENT){
            while(line_start < line.len) : (line_start += 1){
                if(line[line_start] == ']'){
                    curr_state = .NORMAL;
                    line_start += 1;
                    break;
                }
            }
        }

        if(line_start + 1 < line.len){
            if(line[line_start] == 'c' and line[line_start + 1] == '['){
                curr_state = .COMMENT;
                line_start += 2;
            }
        }
        if(line.len == line_start){ continue; }
        
        while(line_start < line.len){
               if(curr_state == .COMMENT){
                    if(line[line_start] == ']'){
                        curr_state = .NORMAL;
                        line_start += 1;
                        continue;
                    }
                    else{
                        line_start += 1; continue;
                    }
               }
               
               if(line[line_start] == '\t'){
                    curr_indent += 1;
                    line_start  += 1;
               }
               else if(line[line_start] == ' '){
                    space_count += 1;
                    line_start  += 1;
                    if(space_count == SPACE_INDENT){ curr_indent += 1; space_count -= SPACE_INDENT;}
               }
               else if(line_start + 1 < line.len and curr_state == .NORMAL){
                    if(line[line_start] == 'c' and line[line_start] != '['){
                        curr_state = .COMMENT;
                        line_start += 2;
                        continue;
                    } else { break; }
            }  else{ break; }
        }
        // if line has nothing other than indentation, just ignore it lol
        if(line_start >= line.len or curr_state == .COMMENT) continue;
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

        var word: []const u8 = undefined;
        var string_start : u32 = 0;
        
        while(true){
            // Do check for ending string in this line
            if(curr_state == .STRING) {
                while(line_start < line.len) : (line_start += 1){
                    if(line[line_start] == '\"'){
                          curr_state = .NORMAL;
                          var replaced :[]u8 = try alloc.dupe(u8,line[string_start..line_start]);
                          var changed : usize = undefined;

                          changed = std.mem.replace(u8,line[string_start..line_start], "\\n", "\n", replaced); replaced.len -= changed;
                          changed = std.mem.replace(u8,replaced, "\\t", "\t", replaced);                       replaced.len -= changed;
                          changed = std.mem.replace(u8,replaced, "\\\"", "\"", replaced);                      replaced.len -= changed;
                          changed = std.mem.replace(u8,replaced, "\\r", "\r", replaced);                       replaced.len -= changed;
                          
                          try token_list.append(Token{.tok = .STRING_LIT, .value = replaced});
                          line_start += 1;
                          break; 
                    }
                }
                if(curr_state == .STRING){  return LexerError.UnclosedString; }
            }
// Getting token and also skipping comments            
            while (line_start < line.len and (line[line_start] == ' ' or line[line_start] == '\t')) : (line_start += 1) {}

            if(line_start + 1 < line.len){
                if(line[line_start] == 'c' and line[line_start+1] == '['){
                    curr_state = .COMMENT;
                    line_start += 2;
                }
                if(curr_state == .COMMENT){
                    while(line_start < line.len and curr_state == .COMMENT) : (line_start += 1){
                        if(line[line_start] == ']'){ curr_state = .NORMAL; if(line_start + 1 < line.len) line_start += 1; }
                    }
                }
            }
            
            const start = line_start;
            if (start == line.len) { break; }

            while (line_start < line.len and !(line[line_start] == ' ' or line[line_start] == '\t')) : (line_start += 1) {}
            const end = line_start;

            word = line[start..end];
// ----------------------------------------------------------------------------------------------------------------------------------

// Check for opetarors, quickly
            if(word.len == 1){
                var is_op : bool = false;
                for(one_char_ops) |op,i| {
                    if(word[0] == op){
                         try token_list.append(Token{.tok = @intToEnum(Tok_enum,i + @enumToInt(Tok_enum.L_PAREN))});
                         is_op = true;
                         break;
                    }
                }
                if(is_op) continue;
            }
// ------------------------------------------------------------------------------------
// Check for keywords and stuff that are easy to find
            if(try checkIfKeyword(&token_list,word,alloc)) continue; // continue getting other tokens in this line if didn't found keyword
// --------------------------------------------------------------------------------------------------------------------

// Check for other stuff that might be cramped together, like operators, literals, parenthesis or whatever            
            while(true){
            var idx : u32 = 1;
            if(word.len == 0) break;
            switch(word[0]){
            '0'...'9' => { // Must be float or int
                var int_or_float : bool = true;
                while(idx < word.len) : (idx += 1){
                    if(word[idx] == '.') { int_or_float = false; }
                    else if (!(word[idx] >= '0' and word[idx] <= '9')){
                        break;
                    }
                }
                try token_list.append(Token{.tok = if(int_or_float) .INT_LIT else .FLOAT_LIT, .value = try alloc.dupe(u8,word[0..idx])} ); 
                word.ptr += idx;
                word.len -= idx;
                continue; 
            },
            '\"' => {
                curr_state = .STRING;
                while(idx < word.len) : (idx += 1){
                    if(word[idx] == '\"' and word[idx-1] != '\\') { break; }
                }
                
                if(idx == word.len) { string_start = start+1;break; } // Didn't found the end of string in current token, maybe its somewhere else in the line

                var replaced : []u8 = try alloc.dupe(u8,word[1..idx]);
                var changed : usize = undefined;
                changed = std.mem.replace(u8,word[1..idx], "\\n", "\n", replaced);  replaced.len -= changed;
                changed = std.mem.replace(u8,replaced, "\\t", "\t", replaced);      replaced.len -= changed;
                changed = std.mem.replace(u8,replaced, "\\\"", "\"", replaced);     replaced.len -= changed;
                changed = std.mem.replace(u8,replaced, "\\r", "\r", replaced);      replaced.len -= changed;
                
                try token_list.append(Token{.tok = .STRING_LIT, .value = replaced});

                word.ptr += idx + 1; // + 1 because of the starting \"
                word.len -= idx + 1;
                curr_state = .NORMAL;
                continue;               
            },
            '\'' => {
                
                 if(idx + 1 < word.len){
                    if(word[idx] == '\\'){ // some slash char
                        idx += 1;
                        var char : ?u8 = switch(word[idx]) {
                            'n' => '\n',
                            't' => '\t',
                            '0' =>  0,
                            '\\'=> '\\',
                            'r'=> '\r',
                            else => null
                        };
                        if(char == null or word[idx+1] != '\''){ return LexerError.IncorrectSlashChar; }
                        
                         try token_list.append(Token{.tok = .CHAR_LIT, .value =  try alloc.dupe(u8 ,&[1]u8{char.?}) });
                         word.ptr += idx + 2; // + 2 because of some char and \'
                         word.len -= idx + 2;
                         continue;
                    }
                    else{
                        if(word[idx + 1] == '\'') {
                            try token_list.append(Token{.tok = .CHAR_LIT, .value = try alloc.dupe(u8 ,&[1]u8{word[1]}) });
                            word.ptr += idx + 2;
                            word.len -= idx + 2; 
                            continue;
                        }
                        else{ return LexerError.IncorrectSlashChar; }              
                    }
                 }
                 else{ return LexerError.IncorrectSlashChar; }
            },
            '-' => {
                if(word.len == 2){ // stupid, since no one will leave arrow as last token, but well, i will leave it here
                    if(word[idx] == '>') try token_list.append(Token{.tok = .ARROW}); break;
                }
                switch(word[idx]){
                    '0'...'9' => {
                        var int_or_float : bool = true;
                        while(idx < word.len) : (idx += 1){
                            if(word[idx] == '.') { int_or_float = false; }
                            else if (!(word[idx] >= '0' and word[idx] <= '9')){
                                idx -= 1;
                                break;
                            }
                        }
                        try token_list.append(Token{.tok = if(int_or_float) .INT_LIT else .FLOAT_LIT, .value = try alloc.dupe(u8,word[0..idx])} ); 
                        word.ptr += idx;
                        word.len -= idx;
                        continue; 
                    },
                    '>' => {
                        try token_list.append(Token{.tok = .ARROW});
                        word.ptr += 2;
                        word.len -= 2;
                    },
                    else => {
                        try token_list.append(Token{.tok = .SUB});
                        word.ptr += 1;
                        word.len -= 1;
                        continue;
                    }
                }
            },
            else => { // no shit found in first letter, so we check if the first letter is operator
                    var is_op : bool = false;
                    for(one_char_ops) |op,i| {
                    if(word[0] == op){
                         try token_list.append(Token{.tok = @intToEnum(Tok_enum,i + @enumToInt(Tok_enum.L_PAREN))});
                         is_op = true;
                         word.ptr += 1;
                         word.len -= 1;
                         break;
                    }
                    }
                    if(is_op) continue; // if operator is found, we continue
            } }
            
            // If its not int/float, nor string, nor char nor starts with operator, we look for operator and append the word we found until operator
            var op_found : bool = false;
            op_search: for(word) |c,i| {
                for(one_char_ops) |op| {
                    if(c == op){
                         if(!(try checkIfKeyword(&token_list,word[0..i],alloc)))
                              try token_list.append(Token{.tok = .IDENTIFIER, .value = try alloc.dupe(u8,word[0..i])});
                         op_found = true;
                         word.ptr += i;
                         word.len -= i;
                         break :op_search;
                    }
            } }
            if(op_found) continue; // if operator is found, we want to continue working on the word

            if(!(try checkIfKeyword(&token_list,word[0..],alloc)))
                 try token_list.append(Token{.tok = .IDENTIFIER, .value = try alloc.dupe(u8,word[0..])});
            break;
            // End of looking for stuff in one word
        }
                        
// ----------------------------------------------------------------------------------------------------------------------------
            
        } // End of the line 
        try token_list.append(Token{.tok = .NEWLINE});        
    }
    if(curr_state == .COMMENT){ return LexerError.UnclosedComment; }

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

pub fn freeTokenValues(tokens : []const Token, alloc: std.mem.Allocator) void {
    for (tokens) |t|{
        if(t.value != null){
            alloc.free(t.value.?);
        }
    }
}

test "lex basic variable declaration" {
    const res = try tokenize("piwo int abcd = -10", test_alloc);
    defer test_alloc.free(res);
    errdefer {
            std.debug.print("\n==========================\n!!!RESULT: {s}\n==========================\n", .{res});
            freeTokenValues(res,test_alloc);
            }
    const test1 = [_]Token{ 
            .{.tok = .PIWO},
            .{.tok = .INT_TYPE}, 
            .{.tok = .IDENTIFIER, .value = "abcd"}, 
            .{.tok = .EQUAL},
            .{.tok = .INT_LIT, .value = "-10"},
            .{.tok = .NEWLINE},
    };
    try testTokens(test1[0..],res);
    freeTokenValues(res,test_alloc);
}
test "lex basic variable assign" {
    const res = try tokenize("abcd = 69.420", test_alloc);
    defer test_alloc.free(res);

    errdefer {
            std.debug.print("\n==========================\n!!!RESULT: {s}\n==========================\n", .{res});
            freeTokenValues(res,test_alloc);
            }
    const test1 = [_]Token{
            .{.tok = .IDENTIFIER, .value = "abcd"},
            .{.tok = .EQUAL},
            .{.tok = .FLOAT_LIT, .value = "69.420"},
            .{.tok = .NEWLINE},
    };
    
    try testTokens(test1[0..],res);
    freeTokenValues(res,test_alloc);
}


test "lex basic indentation" {
    const res = try tokenize("\tpiwo", test_alloc);
    defer test_alloc.free(res); 
    errdefer {
            std.debug.print("\n==========================\n!!!RESULT: {s}\n==========================\n", .{res});
            freeTokenValues(res,test_alloc);
            }

    const test1 = [_]Token{
            .{.tok = .INDENT},
            .{.tok = .PIWO},
            .{.tok = .NEWLINE},
    };
    
    try testTokens(test1[0..],res);
    freeTokenValues(res,test_alloc);
}

test "lex recovering from indentation" {
    const source = \\piwo
                   \\    piwo
                   \\        piwo
                   //\\piwo
                   ;
    const res = try tokenize(source, test_alloc);
    defer test_alloc.free(res);
    errdefer {
            std.debug.print("\n==========================\n!!!RESULT: {s}\n==========================\n", .{res});
            freeTokenValues(res,test_alloc);
            }

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
    freeTokenValues(res,test_alloc);
}

test "lex ignoring line empty with indentation" { 
    const source = \\piwo
                   \\    piwo
                   \\    
                   \\    piwo
                   ;
    const res = try tokenize(source, test_alloc);
    defer test_alloc.free(res);
    errdefer {
            std.debug.print("\n==========================\n!!!RESULT: {s}\n==========================\n", .{res});
            freeTokenValues(res,test_alloc);
            }

    const test1 = [_]Token{
            .{.tok = .PIWO},.{.tok = .NEWLINE},
            .{.tok = .INDENT},
            .{.tok = .PIWO},.{.tok = .NEWLINE},
            .{.tok = .PIWO},.{.tok = .NEWLINE},
            .{.tok = .DEDENT},
    };
    
    try testTokens(test1[0..],res);
    freeTokenValues(res,test_alloc);
}

test "lex unicode polish letters working - 1" {
    const res = try tokenize("jeżeli", test_alloc);
    defer test_alloc.free(res); 
    errdefer {
            std.debug.print("\n==========================\n!!!RESULT: {s}\n==========================\n", .{res});
            freeTokenValues(res,test_alloc);
            }

    const test1 = [_]Token{
            .{.tok = .IF},
            .{.tok = .NEWLINE},
    };
    
    try testTokens(test1[0..],res);
    freeTokenValues(res,test_alloc);
}

test "lex unicode polish letters working - 2" {
    const res = try tokenize("zwróć", test_alloc);
    defer test_alloc.free(res); 
    errdefer {
            std.debug.print("\n==========================\n!!!RESULT: {s}\n==========================\n", .{res});
            freeTokenValues(res,test_alloc);
            }

    const test1 = [_]Token{
            .{.tok = .RETURN},
            .{.tok = .NEWLINE},
    };
    
    try testTokens(test1[0..],res);
    freeTokenValues(res,test_alloc);
}

test "lex skipping comments" {
    const source = \\c[Komentarz]
                   \\    c[Comment]piwo
                   \\piwo c[ fricking spaces ] piwo c[AAAAAAA]
                   ;
    const res = try tokenize(source, test_alloc);
    defer test_alloc.free(res); 
    errdefer {
            std.debug.print("\n==========================\n!!!RESULT: {s}\n==========================\n", .{res});
            freeTokenValues(res,test_alloc);
            }

    const test1 = [_]Token{
            .{.tok = .INDENT},
            .{.tok = .PIWO},.{.tok = .NEWLINE},
            .{.tok = .DEDENT},
            .{.tok = .PIWO},.{.tok = .PIWO},.{.tok = .NEWLINE},
    };
    
    try testTokens(test1[0..],res);
    freeTokenValues(res,test_alloc);
}

test "lex multiline comments" {
    const source = \\c[
                   \\ Everything here is skipped
                   \\]
                   \\piwo
                   ;
    const res = try tokenize(source, test_alloc);
    defer test_alloc.free(res); 
    errdefer {
            std.debug.print("\n==========================\n!!!RESULT: {s}\n==========================\n", .{res});
            freeTokenValues(res,test_alloc);
            }

    const test1 = [_]Token{
            .{.tok = .PIWO},.{.tok = .NEWLINE},
    };
    
    try testTokens(test1[0..],res);
    freeTokenValues(res,test_alloc);
}

test "lex error unclosed comment" {
    const source = \\c[ Oops i forgot to close it 
                   \\
                   \\piwo
                   ;
    
    if(tokenize(source, test_alloc)) |res| {
        _ = res;
        unreachable;
    } else |err| {
        try expect(err == LexerError.UnclosedComment);
    }
}

test "lex string literal" {
    const res = try tokenize("piwo string abcd = \" foo abc\"", test_alloc);
    defer test_alloc.free(res);
    errdefer {
            std.debug.print("\n==========================\n!!!RESULT: {s}\n==========================\n", .{res});
            freeTokenValues(res,test_alloc);
            }
    const test1 = [_]Token{ 
            .{.tok = .PIWO},
            .{.tok = .STRING_TYPE}, 
            .{.tok = .IDENTIFIER, .value = "abcd"}, 
            .{.tok = .EQUAL}, 
            .{.tok = .STRING_LIT, .value = " foo abc"},
            .{.tok = .NEWLINE},
    };
    try testTokens(test1[0..],res);
    freeTokenValues(res,test_alloc);
}

test "lex error unclosed string" {
    if(tokenize("piwo string abcd = \"foo bar spaces ", test_alloc)) |res| {
        _ = res;
        unreachable;
    } else |err| {
        try expect(err == LexerError.UnclosedString);
    }
}

test "lex string slashes support" {
    const res = try tokenize("piwo string abcd = \"\\n\\t\\\"\"", test_alloc);
    defer test_alloc.free(res);
    errdefer {
            std.debug.print("\n==========================\n!!!RESULT: {s}\n==========================\n", .{res});
            freeTokenValues(res,test_alloc);
            }
    const test1 = [_]Token{ 
            .{.tok = .PIWO},
            .{.tok = .STRING_TYPE}, 
            .{.tok = .IDENTIFIER, .value = "abcd"}, 
            .{.tok = .EQUAL}, 
            .{.tok = .STRING_LIT, .value = "\n\t\""},
            .{.tok = .NEWLINE},
    };
    try testTokens(test1[0..],res);
    freeTokenValues(res,test_alloc);
}

test "lex char literals" {
    const res = try tokenize("piwo char abcd = \'\\t\'", test_alloc);
    defer test_alloc.free(res);
    errdefer {
            std.debug.print("\n==========================\n!!!RESULT: {s}\n==========================\n", .{res});
            freeTokenValues(res,test_alloc);
            }
    const test1 = [_]Token{ 
            .{.tok = .PIWO},
            .{.tok = .CHAR_TYPE}, 
            .{.tok = .IDENTIFIER, .value = "abcd"}, 
            .{.tok = .EQUAL}, 
            .{.tok = .CHAR_LIT, .value = "\t"},
            .{.tok = .NEWLINE},
    };
    try testTokens(test1[0..],res);
    freeTokenValues(res,test_alloc);
}

test "lex error incorrect/unsupported char" {
    if(tokenize("piwo char abcd = \'\\q\'", test_alloc)) |res| {
        _ = res;
        unreachable;
    } else |err| {
        try expect(err == LexerError.IncorrectSlashChar);
    }
}

test "lex example code" {
    const source = 
    \\piwo int foo = 69 
    \\wino float bar = 420.12
    \\kufel string tab[] = {"69","b","c"}
    \\jeżeli foo == tab[0]:
    \\    wypisz "Equal\n"
    \\inaczej:
    \\    podaj foo
     ;
    
    const res = try tokenize(source, test_alloc);
    defer test_alloc.free(res); 
    errdefer {
            std.debug.print("\n==========================\n!!!RESULT: {s}\n==========================\n", .{res});
            freeTokenValues(res,test_alloc);
            }

    const test1 = [_]Token{
            .{.tok = .PIWO},.{.tok = .INT_TYPE}, .{.tok = .IDENTIFIER, .value = "foo"}, .{.tok = .EQUAL}, .{.tok = .INT_LIT, .value = "69"},.{.tok = .NEWLINE},
            .{.tok = .WINO},.{.tok = .FLOAT_TYPE}, .{.tok = .IDENTIFIER, .value = "bar"}, .{.tok = .EQUAL}, .{.tok = .FLOAT_LIT, .value = "420.12"},.{.tok = .NEWLINE},
            .{.tok = .KUFEL},.{.tok = .STRING_TYPE}, .{.tok = .IDENTIFIER, .value = "tab"}, .{.tok = .L_BRACK}, .{.tok = .R_BRACK} ,.{.tok = .EQUAL},.{.tok = .L_CURLY}, .{.tok = .STRING_LIT, .value = "69"},.{.tok = .COMMA},.{.tok = .STRING_LIT, .value = "b"},.{.tok = .COMMA},.{.tok = .STRING_LIT, .value = "c"},.{.tok = .R_CURLY},.{.tok = .NEWLINE},
            .{.tok = .IF}, .{.tok = .IDENTIFIER, .value = "foo"}, .{.tok = .EQUAL}, .{.tok = .EQUAL}, .{.tok = .IDENTIFIER, .value = "tab"}, .{.tok = .L_BRACK}, .{.tok = .INT_LIT, .value = "0"}, .{.tok = .R_BRACK}, .{.tok = .COLON}, .{.tok = .NEWLINE},
            .{.tok = .INDENT},.{.tok = .PRINT},.{.tok = .STRING_LIT, .value = "Equal\n"}, .{.tok = .NEWLINE},
            .{.tok = .DEDENT},
            .{.tok = .ELSE}, .{.tok = .COLON}, .{.tok = .NEWLINE},
            .{.tok = .INDENT}, .{.tok = .INPUT}, .{.tok = .IDENTIFIER, .value = "foo"}, .{.tok = .NEWLINE}, .{.tok = .DEDENT}
    };
    
    try testTokens(test1[0..],res);
    freeTokenValues(res,test_alloc);
}
