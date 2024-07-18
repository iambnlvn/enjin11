const std = @import("std");
const Lexer = @import("Lexer.zig");
const Operator = Lexer.Operator;
const Keyword = Lexer.Keyword;
const Token = Lexer.Token;
const Lexem = Lexer.Lexer.Lexems;
const ArrayList = std.ArrayList;
const Entity = @import("Entity.zig").Entity;
const EntityID = @import("EntityID.zig").ID;
const expectEqual = std.testing.expectEqual;

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

pub const IntegerLiteral = struct {
    value: u64,
    isSigned: bool,
    fn new(list: *ArrayList(IntegerLiteral), value: u64, isSigned: bool, moduleIdx: u64) Entity {
        const idx = list.items.len;
        const id = Entity.new(idx, EntityID.Scope.IntegerLiterals, moduleIdx);
        list.append(.{ .value = value, .isSigned = isSigned }) catch unreachable;
        return id;
    }
};
pub const ArrayLiteral = struct {
    elements: []Entity,
    type: u64, //Todo: implement a Type struct
    fn new(list: *ArrayList(ArrayLiteral), elements: []Entity, moduleIdx: u64) Entity {
        const idx = list.items.len;
        const id = Entity.new(idx, EntityID.Scope.ArrayLiterals, moduleIdx);
        list.append(.{
            .elements = elements,
            .type = undefined,
        }) catch unreachable;
        return id;
    }
};
pub const StructLiteral = struct {
    fields: struct {
        names: [][]const u8,
        initilizers: []Entity,
    },
    type: u64, //Todo: implement a Type struct
    fn new(list: *ArrayList(StructLiteral), fieldNames: [][]const u8, fieldExpr: []Entity, moduleIdx: u64) Entity {
        const idx = list.items.len;
        const id = Entity.new(idx, EntityID.Scope.StructLiterals, moduleIdx);
        list.append(.{
            .fields = .{
                .names = fieldNames,
                .initilizers = fieldExpr,
            },
            .type = undefined, //Todo: should be something like  std.mem.zeros(Type)
        }) catch unreachable;
        return id;
    }
};

test "IntegerLiteral.new adds a new IntegerLiteral and returns correct Entity" {
    var list = ArrayList(IntegerLiteral).init(std.testing.allocator);
    defer list.deinit();

    const value: u64 = 42;
    const isSigned: bool = true;
    const moduleIdx: u64 = 1;

    const entity = IntegerLiteral.new(&list, value, isSigned, moduleIdx);
    _ = entity;

    try expectEqual(list.items.len, 1);
    try expectEqual(list.items[0].value, value);
    try expectEqual(list.items[0].isSigned, isSigned);
}
test "IntegerLiteral.new adds multiple IntegerLiterals and returns correct Entities" {
    var list = ArrayList(IntegerLiteral).init(std.testing.allocator);
    defer list.deinit();

    const value1: u64 = 42;
    const isSigned1: bool = true;
    const moduleIdx1: u64 = 1;

    const entity1 = IntegerLiteral.new(&list, value1, isSigned1, moduleIdx1);
    _ = entity1;

    const value2: u64 = 43;
    const isSigned2: bool = false;
    const moduleIdx2: u64 = 2;

    const entity2 = IntegerLiteral.new(&list, value2, isSigned2, moduleIdx2);
    _ = entity2;

    try expectEqual(list.items.len, 2);
    try expectEqual(list.items[0].value, value1);
    try expectEqual(list.items[0].isSigned, isSigned1);
    try expectEqual(list.items[1].value, value2);
    try expectEqual(list.items[1].isSigned, isSigned2);
}
