const std = @import("std");
const Lexer = @import("Lexer.zig");
const Operator = Lexer.Operator;
const Keyword = Lexer.Keyword;
const Token = Lexer.Token;
const Lexem = Lexer.Lexer.Lexems;

const Precedence = enum {
    None,
    Assignment,
    Logical,
    Comparison,
    LightArithmetic, // +, -
    HeavyArithmetic, // mul, div
    Unary,
    Call,
    Declaration,
    Primary,
    fn increment(comptime self: Precedence) Precedence {
        return comptime @as(Precedence, @enumFromInt(@intFromEnum(self) + 1));
    }
};
