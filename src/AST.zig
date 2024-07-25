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
    }
};
