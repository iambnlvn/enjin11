const std = @import("std");
const print = std.debug.print;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const panic = std.debug.panic;
const testing = std.testing;

pub const Operator = packed struct {
    start: u64,
    line: u64,
    column: u64,
    value: ID,
    pub const ID = enum(u8) {
        Declaration,
        LeftParen,
        RightParen,
        LeftBracket,
        RightBracket,
        Dot,
        Plus,
        Minus,
        Div,
        Mul,
        Mod,
        Equal,
        NotEqual,
        LessThan,
        GreaterThan,
        LessThanEqual,
        GreaterThanEqual,
        Constant,
        Assignment,
        MinusAssignment,
        PlusAssignment,
        MulAssignment,
        DivAssignment,
        ModAssignment,
        BitwiseAndAssignment,
        BitwiseOrAssignment,
        BitwiseXorAssignment, // TODO*: implement RightShiftAssignment, LeftShiftAssignment (Experimental and does not seem useful)
        Not,
        And,
        Or,
        Xor,
        ShiftLeft,
        ShiftRight,
        Arrow,
    };
};

pub const Keyword = struct {
    value: ID,
    start: u64,
    line: u64,
    column: u64,

    pub const ID = enum {
        @"if",
        @"else",
        @"while",
        @"for",
        @"break",
        @"continue",
        @"return",
        noreturn,
        @"unreachable",
        void,
        @"switch",
        @"struct",
        @"extern",
    };
};

pub const Token = enum(u8) {
    IntLiteral,
    FloatLiteral,
    CharLiteral,
    StringLiteral,
    Identifier,
    Keyword,
    Sign,
    Operator,
};

pub const Tokenizer = struct {
    currentIdx: u64,
    LineCount: u64,
    tokens: ArrayList(Token),
    Identifiers: ArrayList(Identifier),
    Keywords: ArrayList(Keyword),
    IntLiterals: ArrayList(IntLiteral),
    CharLiterals: ArrayList(CharLiteral),
    StringLiterals: ArrayList(StringLiteral),
    Signs: ArrayList(Sign),
    Operators: ArrayList(Operator),
};

pub const Identifier = struct {
    value: []const u8,
    start: u64,
    line: u64,
    column: u64,
};

pub const IntLiteral = packed struct {
    value: u64,
    start: u64,
    end: u64,
    line: u64,
    column: u64,
};

pub const CharLiteral = packed struct {
    value: u8,
    start: u64,
    line: u64,
    column: u64,
};

pub const Sign = CharLiteral;
pub const StringLiteral = Identifier;

pub fn main() !void {}

pub const Lexer = struct {
    pub const Lexems = struct {
        Tokens: []Token,
        IntLiterals: []IntLiteral,
        CharLiterals: []CharLiteral,
        StringLiterals: []StringLiteral,
        Identifiers: []Identifier,
        Keywords: []Keyword,
        Signs: []Sign,
        Operators: []Operator,
        LineCount: u64,
    };

    pub fn analyze(allocator: *Allocator, src: []const u8) Lexems {
        var tokenizer = Tokenizer{
            .currentIdx = 0,
            .LineCount = 0,
            .tokens = ArrayList(Token).init(allocator.*),
            .IntLiterals = ArrayList(IntLiteral).init(allocator.*),
            .CharLiterals = ArrayList(CharLiteral).init(allocator.*),
            .StringLiterals = ArrayList(StringLiteral).init(allocator.*),
            .Identifiers = ArrayList(Identifier).init(allocator.*),
            .Keywords = ArrayList(Keyword).init(allocator.*),
            .Signs = ArrayList(Sign).init(allocator.*),
            .Operators = ArrayList(Operator).init(allocator.*),
        };

        var currentLineStart: u64 = 0;

        while (tokenizer.currentIdx < src.len) : (tokenizer.currentIdx += 1) {
            const c = src[tokenizer.currentIdx];

            if (c == '/' and src[tokenizer.currentIdx + 1] == '/') {
                var ch = c;
                while (ch != '\n') {
                    tokenizer.currentIdx += 1;
                    ch = src[tokenizer.currentIdx];
                }
                tokenizer.currentIdx -= 1;
                continue;
            }
            var start: u64 = tokenizer.currentIdx;
            var end: u64 = tokenizer.currentIdx;
            const col = @as(u32, @intCast(start - currentLineStart));

            switch (c) {
                // ignore spaces, ret chars, tabs
                '\t', ' ', '\r' => {},
                '\n' => {
                    tokenizer.LineCount += 1;
                    currentLineStart = tokenizer.currentIdx + 1;
                },
                'a'...'z', 'A'...'Z', '_' => {
                    var char = c;
                    while (std.ascii.isAlphabetic(char) or std.ascii.isDigit(char) or char == '_') {
                        tokenizer.currentIdx += 1;
                        char = src[tokenizer.currentIdx];
                    }
                    end = tokenizer.currentIdx;
                    tokenizer.currentIdx -= 1;
                    if (std.meta.stringToEnum(Keyword.ID, src[start..end])) |kw| {
                        tokenizer.Keywords.append(.{
                            .value = kw,
                            .start = start,
                            .line = tokenizer.LineCount,
                            .column = col,
                        }) catch unreachable;
                        tokenizer.tokens.append(.Keyword) catch unreachable;
                    } else {
                        tokenizer.Identifiers.append(.{
                            .value = src[start..end],
                            .start = start,
                            .line = tokenizer.LineCount,
                            .column = col,
                        }) catch unreachable;
                        tokenizer.Identifiers.append(.{
                            .value = src[start..end],
                            .start = start,
                            .line = tokenizer.LineCount,
                            .column = col,
                        }) catch unreachable;
                    }
                },
                '0'...'9' => {
                    var integer = c;
                    while (std.ascii.isDigit(c)) {
                        tokenizer.currentIdx += 1;
                        integer = src[tokenizer.currentIdx];
                    }
                    end = tokenizer.currentIdx;
                    tokenizer.currentIdx -= 1;
                    const num = src[start..end];
                    const val = std.fmt.parseUnsigned(u64, num, 10) catch panic("Failed to parse integer", .{});
                    tokenizer.IntLiterals.append(.{
                        .value = val,
                        .start = start,
                        .end = end,
                        .line = tokenizer.LineCount,
                        .column = col,
                    }) catch unreachable;
                    tokenizer.tokens.append(.IntLiteral) catch unreachable;
                },
                '"' => {
                    while (true) {
                        tokenizer.currentIdx += 1;
                        if (src[tokenizer.currentIdx] == '"') {
                            break;
                        }
                    }
                    end = tokenizer.currentIdx;
                    start += 1;

                    const str = src[start..end];
                    tokenizer.StringLiterals.append(.{
                        .value = str,
                        .start = start,
                        .line = tokenizer.LineCount,
                        .column = col,
                    }) catch unreachable;
                    tokenizer.tokens.append(.StringLiteral) catch unreachable;
                },
                '(' => {
                    tokenizer.Operators.append(.{
                        .value = Operator.ID.LeftParen,
                        .start = start,
                        .line = tokenizer.LineCount,
                        .column = col,
                    }) catch unreachable;
                    tokenizer.tokens.append(.Operator) catch unreachable;
                },
                ')' => {
                    tokenizer.Operators.append(.{
                        .value = Operator.ID.RightParen,
                        .start = start,
                        .line = tokenizer.LineCount,
                        .column = col,
                    }) catch unreachable;
                    tokenizer.tokens.append(.Operator) catch unreachable;
                },
                '[' => {
                    tokenizer.Operators.append(.{
                        .value = Operator.ID.LeftBracket,
                        .start = start,
                        .line = tokenizer.LineCount,
                        .column = col,
                    }) catch unreachable;
                    tokenizer.tokens.append(.Operator) catch unreachable;
                },
                ']' => {
                    tokenizer.Operators.append(.{
                        .value = Operator.ID.RightBracket,
                        .start = start,
                        .line = tokenizer.LineCount,
                        .column = col,
                    }) catch unreachable;
                    tokenizer.tokens.append(.Operator) catch unreachable;
                },
                '.' => {
                    tokenizer.Operators.append(.{
                        .value = Operator.ID.Dot,
                        .start = start,
                        .line = tokenizer.LineCount,
                        .column = col,
                    }) catch unreachable;
                    tokenizer.tokens.append(.Operator) catch unreachable;
                },
                ':' => {
                    tokenizer.tokens.append(.Operator) catch unreachable;
                    if (src[tokenizer.currentIdx + 1] == ':') {
                        tokenizer.currentIdx += 1;
                        tokenizer.Operators.append(.{
                            .value = Operator.ID.Constant,
                            .start = start,
                            .line = tokenizer.LineCount,
                            .column = col,
                        }) catch unreachable;
                    } else {
                        tokenizer.Operators.append(.{
                            .value = Operator.ID.Declaration,
                            .start = start,
                            .line = tokenizer.LineCount,
                            .column = col,
                        }) catch unreachable;
                    }
                },
                '+' => {
                    tokenizer.tokens.append(.Operator) catch unreachable;
                    if (src[tokenizer.currentIdx + 1] == '=') {
                        tokenizer.currentIdx += 1;
                        tokenizer.Operators.append(.{
                            .value = Operator.ID.PlusAssignment,
                            .start = start,
                            .line = tokenizer.LineCount,
                            .column = col,
                        }) catch unreachable;
                    } else {
                        tokenizer.Operators.append(.{
                            .value = Operator.ID.Plus,
                            .start = start,
                            .line = tokenizer.LineCount,
                            .column = col,
                        }) catch unreachable;
                    }
                },
                '-' => {
                    tokenizer.tokens.append(.Operator) catch unreachable;
                    if (src[tokenizer.currentIdx + 1] == '=') {
                        tokenizer.currentIdx += 1;
                        tokenizer.Operators.append(.{
                            .value = Operator.ID.MinusAssignment,
                            .start = start,
                            .line = tokenizer.LineCount,
                            .column = col,
                        }) catch unreachable;
                    } else {
                        tokenizer.Operators.append(.{
                            .value = Operator.ID.Minus,
                            .start = start,
                            .line = tokenizer.LineCount,
                            .column = col,
                        }) catch unreachable;
                    }
                },
                '*' => {
                    tokenizer.tokens.append(.Operator) catch unreachable;
                    if (src[tokenizer.currentIdx + 1] == '=') {
                        tokenizer.currentIdx += 1;
                        tokenizer.Operators.append(.{
                            .value = Operator.ID.MulAssignment,
                            .start = start,
                            .line = tokenizer.LineCount,
                            .column = col,
                        }) catch unreachable;
                    } else {
                        tokenizer.Operators.append(.{
                            .value = Operator.ID.Mul,
                            .start = start,
                            .line = tokenizer.LineCount,
                            .column = col,
                        }) catch unreachable;
                    }
                },
                '/' => {
                    tokenizer.tokens.append(.Operator) catch unreachable;
                    if (src[tokenizer.currentIdx + 1] == '=') {
                        tokenizer.currentIdx += 1;
                        tokenizer.Operators.append(.{
                            .value = Operator.ID.DivAssignment,
                            .start = start,
                            .line = tokenizer.LineCount,
                            .column = col,
                        }) catch unreachable;
                    } else {
                        tokenizer.Operators.append(.{
                            .value = Operator.ID.Div,
                            .start = start,
                            .line = tokenizer.LineCount,
                            .column = col,
                        }) catch unreachable;
                    }
                },
                '%' => {
                    tokenizer.tokens.append(.Operator) catch unreachable;
                    if (src[tokenizer.currentIdx + 1] == '=') {
                        tokenizer.currentIdx += 1;
                        tokenizer.Operators.append(.{
                            .value = Operator.ID.ModAssignment,
                            .start = start,
                            .line = tokenizer.LineCount,
                            .column = col,
                        }) catch unreachable;
                    } else {
                        tokenizer.Operators.append(.{
                            .value = Operator.ID.Mod,
                            .start = start,
                            .line = tokenizer.LineCount,
                            .column = col,
                        }) catch unreachable;
                    }
                },
                '=' => {
                    tokenizer.tokens.append(.Operator) catch unreachable;
                    if (src[tokenizer.currentIdx + 1] == '=') {
                        tokenizer.currentIdx += 1;
                        tokenizer.Operators.append(.{
                            .value = Operator.ID.Equal,
                            .start = start,
                            .line = tokenizer.LineCount,
                            .column = col,
                        }) catch unreachable;
                    } else {
                        tokenizer.Operators.append(.{
                            .value = Operator.ID.Assignment,
                            .start = start,
                            .line = tokenizer.LineCount,
                            .column = col,
                        }) catch unreachable;
                    }
                },

                '<' => {
                    tokenizer.tokens.append(.Operator) catch unreachable;
                    if (src[tokenizer.currentIdx + 1] == '=') {
                        tokenizer.currentIdx += 1;
                        tokenizer.Operators.append(.{
                            .value = Operator.ID.LessThanEqual,
                            .start = start,
                            .line = tokenizer.LineCount,
                            .column = col,
                        }) catch unreachable;
                    } else if (src[tokenizer.currentIdx + 1] == '<') {
                        tokenizer.currentIdx += 1;
                        tokenizer.Operators.append(.{
                            .value = Operator.ID.ShiftLeft,
                            .start = start,
                            .line = tokenizer.LineCount,
                            .column = col,
                        }) catch unreachable;
                    } else {
                        tokenizer.Operators.append(.{
                            .value = Operator.ID.LessThan,
                            .start = start,
                            .line = tokenizer.LineCount,
                            .column = col,
                        }) catch unreachable;
                    }
                },
                '>' => {
                    tokenizer.tokens.append(.Operator) catch unreachable;
                    if (src[tokenizer.currentIdx + 1] == '=') {
                        tokenizer.currentIdx += 1;
                        tokenizer.Operators.append(.{
                            .value = Operator.ID.GreaterThanEqual,
                            .start = start,
                            .line = tokenizer.LineCount,
                            .column = col,
                        }) catch unreachable;
                    } else if (src[tokenizer.currentIdx + 1] == '>') {
                        tokenizer.currentIdx += 1;
                        tokenizer.Operators.append(.{
                            .value = Operator.ID.ShiftRight,
                            .start = start,
                            .line = tokenizer.LineCount,
                            .column = col,
                        }) catch unreachable;
                    } else {
                        tokenizer.Operators.append(.{
                            .value = Operator.ID.GreaterThan,
                            .start = start,
                            .line = tokenizer.LineCount,
                            .column = col,
                        }) catch unreachable;
                    }
                },
                ';', ',', '{', '}' => {
                    tokenizer.Signs.append(.{
                        .value = src[tokenizer.currentIdx],
                        .start = start,
                        .line = tokenizer.LineCount,
                        .column = col,
                    }) catch unreachable;
                },
                '!' => {
                    tokenizer.tokens.append(.Operator) catch unreachable;
                    if (src[tokenizer.currentIdx + 1] == '=') {
                        tokenizer.currentIdx += 1;
                        tokenizer.Operators.append(.{
                            .value = Operator.ID.NotEqual,
                            .start = start,
                            .line = tokenizer.LineCount,
                            .column = col,
                        }) catch unreachable;
                    } else {
                        tokenizer.Operators.append(.{
                            .value = Operator.ID.Not,
                            .start = start,
                            .line = tokenizer.LineCount,
                            .column = col,
                        }) catch unreachable;
                    }
                },
                '&' => {
                    tokenizer.tokens.append(.Operator) catch unreachable;
                    if (src[tokenizer.currentIdx + 1] == '&') {
                        tokenizer.currentIdx += 1;
                        tokenizer.Operators.append(.{
                            .value = Operator.ID.And,
                            .start = start,
                            .line = tokenizer.LineCount,
                            .column = col,
                        }) catch unreachable;
                    } else if (src[tokenizer.currentIdx + 1] == '=') {
                        tokenizer.currentIdx += 1;
                        tokenizer.Operators.append(.{
                            .value = Operator.ID.BitwiseAndAssignment,
                            .start = start,
                            .line = tokenizer.LineCount,
                            .column = col,
                        }) catch unreachable;
                    } else {
                        panic("Damn, we did not implement : {c}", .{src[tokenizer.currentIdx]});
                    }
                },
                '|' => {
                    tokenizer.tokens.append(.Operator) catch unreachable;
                    if (src[tokenizer.currentIdx + 1] == '|') {
                        tokenizer.currentIdx += 1;
                        tokenizer.Operators.append(.{
                            .value = Operator.ID.Or,
                            .start = start,
                            .line = tokenizer.LineCount,
                            .column = col,
                        }) catch unreachable;
                    } else if (src[tokenizer.currentIdx + 1] == '=') {
                        tokenizer.currentIdx += 1;
                        tokenizer.Operators.append(.{
                            .value = Operator.ID.BitwiseOrAssignment,
                            .start = start,
                            .line = tokenizer.LineCount,
                            .column = col,
                        }) catch unreachable;
                    } else {
                        panic("Damn, we did not implement : {c}", .{src[tokenizer.currentIdx]});
                    }
                },
                '^' => {
                    tokenizer.tokens.append(.Operator) catch unreachable;
                    tokenizer.Operators.append(.{
                        .value = Operator.ID.Xor,
                        .start = start,
                        .line = tokenizer.LineCount,
                        .column = col,
                    }) catch unreachable;
                },

                else => {
                    panic("Damn, we did not implement : {c}", .{src[tokenizer.currentIdx]});
                },
            }
        }
        tokenizer.LineCount += 1;

        return Lexems{
            .Tokens = tokenizer.tokens.items,
            .IntLiterals = tokenizer.IntLiterals.items,
            .CharLiterals = tokenizer.CharLiterals.items,
            .StringLiterals = tokenizer.StringLiterals.items,
            .Identifiers = tokenizer.Identifiers.items,
            .Keywords = tokenizer.Keywords.items,
            .Signs = tokenizer.Signs.items,
            .Operators = tokenizer.Operators.items,
            .LineCount = tokenizer.LineCount,
        };
    }
    pub fn printTokens(lexems: Lexems) void {
        for (lexems.Tokens, 0..) |token, idx| {
            print("Token: {}, AT: {d}\n", .{ token, idx });
        }
    }
};

test "Lexer" {
    const src = "struct { \n return ; \n }";
    var allocator = std.heap.page_allocator;
    const lexems = Lexer.analyze(&allocator, src);
    // print("LEXEMS: {any}", .{lexems});

    try testing.expectEqual(lexems.LineCount, 3);
    Lexer.printTokens(lexems);
}
test "Lexer string" {
    const src = "const a = \"Hey mom\";";
    var allocator = std.heap.page_allocator;
    const lexems = Lexer.analyze(&allocator, src);
    try testing.expectEqual(lexems.LineCount, 1);
    try testing.expectEqualStrings(lexems.StringLiterals[0].value, "Hey mom");
    try testing.expectEqual(lexems.StringLiterals[0].start, 11);
    try testing.expectEqual(lexems.StringLiterals[0].line, 0);
    try testing.expectEqual(lexems.StringLiterals[0].column, 10);
}
