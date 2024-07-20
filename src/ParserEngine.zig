const std = @import("std");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const Function = Parser.Function;
const Precedence = Parser.Precedence;
const TokenTypeMap = @import("Parser.zig").TokenTypeMap;
const Module = @import("Module").Module;
const Allocator = std.mem.Allocator;
const Entity = @import("Entity.zig").Entity;
const Operator = Lexer.Operator;
const EntityID = @import("EntityID.zig").ID;
const ArrayList = std.ArrayList;

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

    fn getAndConsume(self: *Self, comptime token: Lexer.Token) TokenTypeMap[@intFromEnum(token)] {
        self.consumeToken(token);
        return self.getToken(token);
    }

    fn parseArithmeticExpr(self: *Self, leftExpr: Entity, operator: Operator.ID, precedence: Precedence) Entity {
        const arithmeticId = switch (operator) {
            .Plus => Parser.ArithmeticExpr.ID.Add,
            .Minus => Parser.ArithmeticExpr.ID.Sub,
            .Mul => Parser.ArithmeticExpr.ID.Mul,
            .Div => Parser.ArithmeticExpr.ID.Div,
            .Mod => Parser.ArithmeticExpr.ID.Mod,
            else => std.debug.panic("Invalid operator"),
        };

        const rightExpr = self.parsePrecedence(@as(Precedence, @enumFromInt(@intFromEnum(precedence) + 1)));
        const arithmeticExpr = Parser.ArithmeticExpr{
            .id = arithmeticId,
            .left = leftExpr,
            .right = rightExpr,
        };

        const currentScopeIdx = self.fnBuilder.currentScope;
        const currentScope = &self.fnBuilder.scopeBuilders.items[currentScopeIdx];

        const arithmeticExprIdx = currentScope.ArithmeticExprs.items.len;
        currentScope.ArithmeticExprs.append(arithmeticExpr) catch unreachable;

        return Entity.new(arithmeticExprIdx, EntityID.Scope.ArithmeticExpr, self.fnBuilder.currentScope);
    }

    fn parsePrecedence(self: *Self, precedence: Precedence) Entity {
        const prefix = self.lexer.tokens[self.lexer.nextIdx];

        const leftExpr = blk: {
            switch (prefix) {
                .Identifier => break :blk self.parsePrefixIdentifier(),
                .IntLiteral => {
                    const signedness = false;
                    const intValue = self.getAndConsume(.IntLiteral).value;
                    break :blk Parser.IntegerLiteral.new(&self.moduleBuilder.intLiterals, intValue, signedness, self.moduleBuilder.idx);
                },
            }
        };
        return self.parseInfix(leftExpr, precedence);
    }

    fn parsePrefixIdentifier(self: *Self) Entity {
        const name = self.getAndConsume(.Identifier).value;
        return Parser.newIdentifierExpr(&self.fnBuilder, name);
    }

    fn parseInfix(self: *Self, precedence: Precedence, precedentLeftExpr: Entity) Entity {
        var leftExpr = precedentLeftExpr;
        var hasLessPrecedence = true;
        while (hasLessPrecedence) {
            const token = self.lexer.tokens[self.lexer.nextIdx];
            const newPrecedence = switch (token) {
                .Sign => signBlk: {
                    const sign = self.getToken(.Sign);
                    const signPrecedence = switch (sign.value) {
                        '{', '}', ';', ',' => Precedence.None,
                        else => std.debug.panic("Invalid precedence!"),
                    };
                    break :signBlk signPrecedence;
                },
                .Operator => operatorBlk: {
                    const operator = self.getToken(.Operator);
                    const operatorPrecedence = switch (operator.value) {
                        .RightParen, .RightBracket => Precedence.None,
                        .Plus, .Minus => Precedence.LightArithmetic,
                        .Mul, .Div, .Mod => Precedence.HeavyArithmetic,
                        .Equal, .GreaterThan, .LessThan, .GreaterThanOrEqual, .LessThanOrEqual, .NotEqual => Precedence.Comparison,
                        .Assignment => Precedence.Assignment,
                        .Declaration => Precedence.Declaration,
                        .LeftParen, .LeftBracket => Precedence.Call,
                        else => std.debug.panic("Invalid precedence!"),
                    };
                    break :operatorBlk operatorPrecedence;
                },
                else => std.debug.panic("Unimplemented token! {any}", .{token}),
            };
            hasLessPrecedence = @intFromEnum(precedence) <= @intFromEnum(newPrecedence);

            if (hasLessPrecedence) {
                const op = self.getAndConsume(.Operator).value;
                leftExpr = switch (op) {
                    .Equal, .GreaterThan, .LessThan, .GreaterThanOrEqual, .LessThanOrEqual, .NotEqual => {
                        self.parseComparison(leftExpr, op, newPrecedence);
                    },
                    .Plus, .Minus, .Mul, .Div, .Mod => {
                        self.parseArithmeticExpr(leftExpr, op, newPrecedence);
                    },
                    .Assignment => {
                        self.parseAssignment(leftExpr, newPrecedence);
                    },
                    .LeftParen => blk: {
                        var argsLeftToParse = self.lexer.tokens[self.lexer.nextIdx] != .Operator or self.getToken(.Operator).value != .RightParen;
                        var argsList = ArrayList(Entity).init(self.allocator);

                        while (argsLeftToParse) {
                            const argId = self.parseExpr();
                            argsList.append(argId) catch unreachable;

                            // to check if there are more arguments to parse
                            const finalToken = self.lexer.tokens[self.lexer.nextIdx];
                            argsLeftToParse = !(finalToken == .Operator and self.getToken(.Operator).value == .RightParen);

                            if (argsLeftToParse) {
                                if (finalToken == .Sign and self.getToken(.Sign).value == ',') {
                                    self.consumeToken(.Sign);
                                    continue;
                                }
                            } else {
                                std.debug.panic("Expected ',' or ')' but got {any}", .{finalToken});
                            }
                        }

                        if (self.getAndConsume(.Operator).value != .RightParen) {
                            std.debug.panic("Expected ')' but got {any}", .{self.lexer.tokens[self.lexer.nextIdx]});
                        }
                        break :blk Parser.InvokeExpr.new(&self.fnBuilder, argsList.items, leftExpr);
                    },
                    //Todo: Implement the rest of the operators
                    else => std.debug.panic("Invalid operator! {any}", .{op}),
                };
            }
        }
        return leftExpr;
    }

    fn parseComparison(self: *Self, leftExpr: Entity, operator: Operator.ID, precedence: Precedence) Entity {
        const compId = switch (operator) {
            .Equal => Parser.Comparison.ID.Equal,
            .GreaterThan => Parser.Comparison.ID.GreaterThan,
            .LessThan => Parser.Comparison.ID.LessThan,
            .GreaterThanOrEqual => Parser.Comparison.ID.GreaterThanOrEqual,
            .LessThanOrEqual => Parser.Comparison.ID.LessThanOrEqual,
            .NotEqual => Parser.Comparison.ID.NotEqual,
            else => std.debug.panic("Invalid operator"),
        };
        const rightExpr = self.parsePrecedence(@as(Precedence, @enumFromInt(@intFromEnum(precedence) + 1)));
        return Parser.Comparison.new(&self.fnBuilder, compId, leftExpr, rightExpr);
    }

    fn parseAssignment(self: *Self, leftExpr: Entity, precedence: Precedence) Entity {
        const rightExpr = self.parsePrecedence(@as(Precedence, @enumFromInt(@intFromEnum(precedence) + 1)));
        return Parser.Assignment.new(&self.fnBuilder, leftExpr, rightExpr);
    }

    fn parseExpr(self: *Self) Entity {
        return self.parsePrecedence(Precedence.Assignment);
    }
};
