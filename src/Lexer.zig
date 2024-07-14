const std = @import("std");
const print = std.debug.print;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const panic = std.debug.panic;

pub const Operator = struct {
    start: u64,
    line: u64,
    column: u64,
    value: ID,
    const ID = enum(u8) {
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
        ShiftLeft,
        ShiftRight,
    };
};

pub const Keyword = struct {
    value: ID,
    start: u64,
    line: u64,
    column: u64,

    const ID = enum {
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
    };
};

pub const Token = enum(u8) {
    intLiteral,
    floatLiteral,
    charLiteral,
    stringLiteral,
    identidier,
    keyword,
    sign,
    operator,
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

pub const IntLiteral = struct {
    value: u64,
    start: u64,
    end: u64,
    line: u64,
    column: u64,
};

pub const CharLiteral = struct {
    value: u8,
    start: u64,
    line: u64,
    column: u64,
};

pub const Sign = CharLiteral;
pub const StringLiteral = Identifier;

pub fn main() !void {}

const Lexer = struct {
    const Lexems = struct {
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

    fn analyze(allocator: Allocator, src: []const u8) Lexems {
        // const Self = @This();

        var tokenizer = Tokenizer{
            .currentIdx = 0,
            .LineCount = 1,
            .tokens = ArrayList(Token).init(allocator),
            .int_literals = ArrayList(IntLiteral).init(allocator),
            .char_literals = ArrayList(CharLiteral).init(allocator),
            .string_literals = ArrayList(StringLiteral).init(allocator),
            .identifiers = ArrayList(Identifier).init(allocator),
            .keywords = ArrayList(Keyword).init(allocator),
            .signs = ArrayList(Sign).init(allocator),
            .operators = ArrayList(Operator).init(allocator),
        };

        const currentLineStart: u64 = 0;

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
            const start: u64 = tokenizer.currentIdx;
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
                    if (std.meta.stringToEnum(Keyword, src[start..end])) |kw| {
                        tokenizer.Keywords.append(.{
                            .value = kw,
                            .start = start,
                            .line = tokenizer.LineCount,
                            .column = col,
                        }) catch unreachable;
                        tokenizer.tokens.append(.keyword) catch unreachable;
                    } else {
                        tokenizer.Identifiers.append(.{
                            .value = src[start..end],
                            .start = start,
                            .line = tokenizer.LineCount,
                            .column = col,
                        }) catch unreachable;
                        tokenizer.Identifiers.append(.identidier) catch unreachable;
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
                    const val = std.fmt.parseUnsigned(u64, num) catch panic("Failed to parse integer", .{});
                    tokenizer.IntLiterals.append(.{
                        .value = val,
                        .start = start,
                        .end = end,
                        .line = tokenizer.lineCount,
                        .column = col,
                    }) catch unreachable;
                    tokenizer.tokens.append(.intLiteral) catch unreachable;
                },
                '(' => {
                    tokenizer.Operators.append(.{
                        .value = Operator.ID.LeftParen,
                        .start = start,
                        .line = tokenizer.LineCount,
                        .column = col,
                    }) catch unreachable;
                    tokenizer.tokens.append(.operator) catch unreachable;
                },
                ')' => {
                    tokenizer.Operators.append(.{
                        .value = Operator.ID.RightParen,
                        .start = start,
                        .line = tokenizer.LineCount,
                        .column = col,
                    }) catch unreachable;
                    tokenizer.tokens.append(.operator) catch unreachable;
                },
                '[' => {
                    tokenizer.Operators.append(.{
                        .value = Operator.ID.LeftBracket,
                        .start = start,
                        .line = tokenizer.LineCount,
                        .column = col,
                    }) catch unreachable;
                    tokenizer.tokens.append(.operator) catch unreachable;
                },
                ']' => {
                    tokenizer.Operators.append(.{
                        .value = Operator.ID.RightBracket,
                        .start = start,
                        .line = tokenizer.LineCount,
                        .column = col,
                    }) catch unreachable;
                    tokenizer.tokens.append(.operator) catch unreachable;
                },
                '.' => {
                    tokenizer.Operators.append(.{
                        .value = Operator.ID.Dot,
                        .start = start,
                        .line = tokenizer.LineCount,
                        .column = col,
                    }) catch unreachable;
                    tokenizer.tokens.append(.operator) catch unreachable;
                },
                ':' => {
                    tokenizer.tokens.append(.operator) catch unreachable;
                    if (src[Tokenizer.currentIdx + 1] == ':') {
                        tokenizer.currentIdx += 1;
                        tokenizer.Operators.append(.{
                            .value = Operator.ID.Constant,
                            .start = start,
                            .line = tokenizer.LineCount,
                            .column = col,
                        }) catch unreachable;
                    } else {
                        tokenizer.tokens.append(.{
                            .value = Operator.ID.Declaration,
                            .start = start,
                            .line = tokenizer.LineCount,
                            .column = col,
                        }) catch unreachable;
                    }
                },
                '+' => {
                    tokenizer.tokens.append(.operator) catch unreachable;
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
                    tokenizer.tokens.append(.operator) catch unreachable;
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
                    tokenizer.tokens.append(.operator) catch unreachable;
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
                    tokenizer.tokens.append(.operator) catch unreachable;
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
                    tokenizer.tokens.append(.operator) catch unreachable;
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
                    tokenizer.tokens.append(.operator) catch unreachable;
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
                    tokenizer.tokens.append(.operator) catch unreachable;
                    if (Tokenizer[Tokenizer.currentIdx + 1] == '=') {
                        tokenizer.currentIdx += 1;
                        tokenizer.Operators.append(.{
                            .value = Operator.ID.LessThanEqual,
                            .start = start,
                            .line = tokenizer.LineCount,
                            .column = col,
                        }) catch unreachable;
                    } else if (Tokenizer[Tokenizer.currentIdx + 1] == '<') {
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
                    tokenizer.tokens.append(.operator) catch unreachable;
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

                else => {
                    panic("Damn, we did not implement : {c}", .{src[tokenizer.currentIdx]});
                },
            }
        }
    }
};
