const std = @import("std");
const ArrayList = std.ArrayList;

const Tok_enum = enum(u8) {
    UNKNOWN,
    NEWLINE,
    // Literals
    STRING_LIT,
    INT_LIT,
    CHAR_LIT,
    FLOAT_LIT,
    BOOL_LIT,
    // Types
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
};

fn readFileToString(path: []const u8, allocator: std.mem.Allocator) ![]u8 {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    const expected_max_size = 2_000_000;
    return try file.reader().readAllAlloc(allocator, expected_max_size);
}
// TODO Maybe make tokenize take string as input instead of a file name

pub fn tokenize(buffer: []const u8, alloc: std.mem.Allocator) ![]Tok_enum {
    var token_list = ArrayList(Tok_enum).init(alloc);
    defer token_list.deinit();

    var it = std.mem.tokenize(u8, buffer, " \t");
    var word: []const u8 = undefined;
    while (true) {
        const t = it.next();

        if (t == null) { break; }
        else { word = t.?; }

        if (std.mem.eql(u8, word, "piwo")) {
            //
            try token_list.append(.PIWO);
        } else if (std.mem.eql(u8, word, "int")) {
            //
            try token_list.append(.INT_TYPE);
        } else if (std.mem.eql(u8, word, "=")) {
            try token_list.append(.EQUAL);
        } else {
            //
            try token_list.append(.UNKNOWN);
        }
    }

    return token_list.toOwnedSlice();
}

const eql = std.mem.eql;
const expect = std.testing.expect;
const test_alloc = std.testing.allocator; // Testing allocator, maybe no need for gpa

test "basic variable declaration" {
    const res = try tokenize("piwo int abcd = 10\n", test_alloc);
    defer test_alloc.free(res); // TODO Change the unknowns to proper tokens
    try expect(eql(Tok_enum, &[_]Tok_enum{ .PIWO, .INT_TYPE, .UNKNOWN, .EQUAL, .UNKNOWN }, res));
}

test "basic variable assign" {
    const res = try tokenize("abcd = 69.420\n", test_alloc);
    defer test_alloc.free(res); // TODO Change the unknowns to proper tokens
    try expect(eql(Tok_enum, &[_]Tok_enum{ .UNKNOWN, .EQUAL, .UNKNOWN }, res));
}
