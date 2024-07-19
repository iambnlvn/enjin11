const std = @import("std");
const Lexer = @import("Lexer.zig");
const Function = @import("Parser.zig").Function;
const TokenTypeMap = @import("Parser.zig").TokenTypeMap;
const Module = @import("Module");
const Allocator = std.mem.Allocator;

pub const ParserEngine = struct {
    const Self = @This();
    const countersType = [std.enums.values(Lexer.Token).len]u32;

    allocator: *Allocator,
    fnBuilder: Function.Builder,
    moduleBuilder: Module.Builder,
    lexer: TokenWalker,

    const TokenWalker = struct {
        nextIdx: u64,
        counters: countersType,
        tokens: []Lexer.Token,
        intLiterals: []Lexer.IntegerLiteral,
        charLiterals: []Lexer.CharLiteral,
        stringLiterals: []Lexer.StringLiteral,
        identifiers: []Lexer.Identifier,
        keywords: []Lexer.Keyword,
        signs: []Lexer.Sign,
        operators: []Lexer.Operator,
    };

    fn getToken(self: *Self, comptime token: Lexer.Token) TokenTypeMap[@intFromEnum(token)] {
        const idx = self.lexer.counters[@intFromEnum(token)];
        comptime switch (token) {
            .IntLiteral => return self.lexer.intLiterals[idx],
            .CharLiteral => return self.lexer.charLiterals[idx],
            .StringLiteral => return self.lexer.stringLiterals[idx],
            .Identifier => return self.lexer.identifiers[idx],
            .Keyword => return self.lexer.keywords[idx],
            .Sign => return self.lexer.signs[idx],
            .Operator => return self.lexer.operators[idx],
            else => unreachable,
        };
    }
    // I'm probably overusing the `comptime` kw, I'll figure it out later
    fn consumeToken(self: *Self, comptime token: Lexer.Token) void {
        self.lexer.counters[@intFromEnum(token)] += 1;
        self.lexer.nextIdx += 1;
    }
    fn isCorrectToken(self: *Self, comptime token: Lexer.Token) bool {
        return (token == self.lexer.tokens[self.lexer.nextIdx]);
    }
};
