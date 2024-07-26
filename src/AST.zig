const Module = @import("Module.zig").Module;
const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Entity = @import("Entity.zig").Entity;
const EntityId = @import("EntityID.zig").ID;
const Lexer = @import("Lexer.zig").Lexer;
const ParserEngine = @import("ParserEngine.zig").ParserEngine;
const Parser = @import("Parser.zig");
const Type = @import("Type.zig");
pub const AST = struct {
    modules: [*]Module,
    moduleNames: [*][]const u8,
    moduleDirs: [*][]const u8,
    moduleLen: u64,
    moduleCap: u64,

    const Self = @This();
    fn parse(allocator: *Allocator, sourceFileName: []const u8, target: std.Target) Self {
        const minModuleCount = 8;
        var ast = Self{
            .modules = (allocator.alloc(Module, minModuleCount) catch unreachable).ptr,
            .moduleNames = (allocator.alloc([]const u8, minModuleCount) catch unreachable).ptr,
            .moduleDirs = (allocator.alloc([]const u8, minModuleCount) catch unreachable).ptr,
            .moduleLen = 0,
            .moduleCap = minModuleCount,
        };
        //parse modules from sourceFileName depending on the target
        return ast;
    }

    fn parseModule(self: *AST, allocator: *Allocator, sourceFile: []const u8, target: std.Target, parentModule: ?Entity) Entity {
        const moduleIdx = self.moduleLen;
        self.moduleLen += 1;

        const newCap = moduleIdx + 1;
        var safeCap = self.moduleCap;

        if (newCap > safeCap) {
            while (newCap > safeCap) {
                safeCap += safeCap / 2 + 8;
            }
            self.modules = (allocator.realloc(self.modules[0..self.moduleLen], safeCap) catch unreachable).ptr;
            self.moduleNames = (allocator.realloc(self.moduleNames[0..self.moduleLen], safeCap) catch unreachable).ptr;
            self.moduleDirs = (allocator.realloc(self.moduleDirs[0..self.moduleLen], safeCap) catch unreachable).ptr;
            self.moduleCap = safeCap;
        }
        const moduleId = Entity.new(moduleIdx, EntityId.Global.Modules, 0);

        const fileContent = if (parentModule) |parentModuleId| blk: {
            const parentDirName = self.moduleDirs[parentModuleId.getArrayIndex()];
            var parentDirHand = std.fs.openDirAbsolute(parentDirName, .{}) catch unreachable;
            defer parentDirHand.close();
            const absPath = parentDirHand.realpathAlloc(allocator, sourceFile) catch unreachable;
            self.moduleDirs[moduleIdx] = std.fs.path.dirname(absPath).?;
            const fileHand = std.fs.openFileAbsolute(absPath, .{}) catch unreachable;
            defer fileHand.close();
            break :blk fileHand.readToEndAlloc(allocator, 0xffffffff) catch unreachable;
        } else blk: {
            const absPath = std.fs.realpathAlloc(allocator, sourceFile) catch unreachable;
            self.moduleDirs[moduleIdx] = std.fs.path.dirname(absPath).?;
            const fileHand = std.fs.openFileAbsolute(absPath, .{}) catch unreachable;
            defer fileHand.close();
            break :blk fileHand.realpathAlloc(allocator, 0xffffffff) catch unreachable;
        };
        const lexer = Lexer.analyze(allocator, fileContent);
        var parser = ParserEngine{ .allocator = allocator, .fnBuilder = undefined, .lexer = .{
            .nextIdx = 0,
            .counters = std.mem.zeroes(ParserEngine.countersType),
            .tokens = lexer.Tokens,
            .intLiterals = lexer.IntLiterals,
            .charLiterals = lexer.CharLiterals,
            .stringLiterals = lexer.StringLiterals,
            .identifiers = lexer.Identifiers,
            .keywords = lexer.Keywords,
            .signs = lexer.Signs,
            .operators = lexer.Operators,
        }, .moduleBuilder = .{
            .internalFns = ArrayList(Parser.Function.Internal).init(allocator),
            .intLiterals = ArrayList(Parser.IntegerLiteral).init(allocator),
            .stringLiterals = ArrayList(Parser.StructLiteral).init(allocator),
            .arrayLiterals = ArrayList(Parser.ArrayLiteral).init(allocator),
            .unresolvedTypes = ArrayList(Parser.unresolvedTypes).init(allocator),
            .sliceTypes = ArrayList(Type.Slice).init(allocator),
            .fnTypes = ArrayList(Type.Function).init(allocator),
            .arrayTypes = ArrayList(Type.Array).init(allocator),
            .structTypes = ArrayList(Type.Struct).init(allocator),
            .libNames = ArrayList([]const u8).init(allocator),
            .idx = @as(u32, @intCast(moduleIdx)),
        } };

        const tokenCount = parser.lexer.tokens.len;
        while (parser.lexer.nextIdx < tokenCount) {
            const topLevelDeclarationNameTOken = parser.lexer.tokens[parser.lexer.nextIdx];
            if (topLevelDeclarationNameTOken != .Identifier) {
                std.debug.panic("Top level declaration must start with an identifier", .{});
            }

            const tldName = parser.getAndConsume(.Identifier).value;
            if (parser.lexer.nextIdx + 2 >= parser.lexer.tokens.len) {
                std.debug.panic("Unexpected end of file", .{});
            }

            const nextToken = parser.lexer.tokens[parser.lexer.nextIdx];

            if (nextToken == .Operator) {
                const operator = parser.getAndConsume(.Operator);

                if (operator.value == .Constant) {
                    const afterConstToken = parser.lexer.tokens[parser.lexer.nextIdx];

                    if (afterConstToken == .Operator) {
                        const afterConstOp = parser.getAndConsume(.Operator);
                        if (afterConstOp.value == .LeftParen) {
                            const leftParenToken = parser.lexer.tokens[parser.lexer.nextIdx];
                            var argNameList = ArrayList([]const u8).init(parser.allocator);
                            var argTypeList = ArrayList(Type).init(parser.allocator);
                            var argsLeft: bool = blk: {
                                if (leftParenToken == .Operator) {
                                    const leftParenNextOp = parser.getToken(.Operator);
                                    if (leftParenNextOp.value == .RightParen) {
                                        break :blk false;
                                    }
                                }
                                break :blk true;
                            };
                            while (argsLeft) {
                                if (parser.lexer.tokens[parser.lexer.nextIdx] != .Identifier) {
                                    std.debug.panic("Expected identifier, Expected argument name, found: {any}", .{parser.lexer.tokens[parser.lexer.nextIdx]});
                                }
                                const argName = parser.getAndConsume(.Identifier).value;
                                const colonTypeToken = parser.lexer.tokens[parser.lexer.nextIdx];
                                if ((colonTypeToken == .Operator and parser.getToken(.Operator).value != .Declaration) or colonTypeToken != .Operator) {
                                    std.debug.panic("Expected colon, Expected argument type, found: {any}", .{colonTypeToken});
                                }
                                parser.consumeToken(.Operator);
                                const argType = parser.parseType();
                                argNameList.append(argName) catch unreachable;
                                argTypeList.append(argType) catch unreachable;

                                const afterArgToken = parser.lexer.tokens[parser.lexer.nextIdx];
                                argsLeft = !(afterArgToken == .Operator and parser.getToken(.Operator).value == .RightParen);
                                if (argsLeft) {
                                    if (afterArgToken == .Sign and parser.getToken(.Sign).value == .Comma) {
                                        parser.consumeToken(.Sign);
                                        continue;
                                    } else {
                                        std.debug.panic("Expected comma, Expected argument separator, found: {any}", .{afterArgToken});
                                    }
                                }
                                if (!(parser.lexer.tokens[parser.lexer.nextIdx] == .Operator and parser.getToken(.Operator).value == .RightParen)) {
                                    std.debug.panic("Expected right parenthesis, Expected end of argument list, found: {any}", .{parser.lexer.tokens[parser.lexer.nextIdx]});
                                }
                                parser.consumeToken(.Operator);
                                var returnType: Type = undefined;
                                if (parser.lexer.tokens[parser.lexer.nextIdx] == .Operator and parser.getToken(.Operator).value == .Arrow) {
                                    parser.consumeToken(.Operator);
                                    returnType = parser.parseType();
                                } else {
                                    returnType = Type.Builtin.voidType;
                                }
                            }
                        }
                    }
                }
            }
        }
    }
};
