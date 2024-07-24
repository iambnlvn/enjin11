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
const Type = @import("Type.zig");
const Scopes = EntityID.Scope;
const Allocator = std.mem.Allocator;
const IR = @import("IR.zig");

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
    type: Type,
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
pub const ArraySubExpr = struct {
    expr: Entity,
    idx: Entity,
    fn new(fnBuilder: *Function.Builder, arrayExpr: Entity, arrayIdxExpr: Entity) Entity {
        const currentScope = &fnBuilder.scopeBuilders.items[fnBuilder.currentScope];

        const idx = currentScope.ArraySubExprs.items.len;

        currentScope.ArraySubExprs.append(.{
            .expr = arrayExpr,
            .idx = arrayIdxExpr,
        }) catch unreachable;
        return Entity.new(idx, Scopes.ArraySubscriptExpr, fnBuilder.currentScope);
    }
};
pub const FieldAccessExpr = struct {
    leftExpr: Entity,
    fieldExpr: Entity,
    type: Type,

    fn new(fnBuilder: *Function.Builder, leftExpr: Entity, fieldExpr: Entity) Entity {
        const currentScope = &fnBuilder.scopeBuilders.items[fnBuilder.currentScope];

        const idx = currentScope.FieldAccessExpr.items.len;

        currentScope.FieldAccessExpr.append(.{
            .leftExpr = leftExpr,
            .fieldExpr = fieldExpr,
            .type = Type.UnresolvedType,
        }) catch unreachable;
        return Entity.new(idx, Scopes.FieldAccessExpr, fnBuilder.currentScope);
    }
};
// TODO: new function is repetitive, refactor to use a factory function

pub const StructLiteral = struct {
    fields: struct {
        names: [][]const u8,
        initilizers: []Entity,
    },
    type: Type,
    fn new(list: *ArrayList(StructLiteral), fieldNames: [][]const u8, fieldExpr: []Entity, moduleIdx: u64) Entity {
        const idx = list.items.len;
        const id = Entity.new(idx, EntityID.Scope.StructLiterals, moduleIdx);
        list.append(.{
            .fields = .{
                .names = fieldNames,
                .initilizers = fieldExpr,
            },
            .type = std.mem.zeros(Type),
        }) catch unreachable;
        return id;
    }
};

pub const Function = struct {
    name: []const u8,
    argNames: [][]const u8,
    type: Type.Function,

    const Internal = struct {
        declaration: Function,
        scopes: []Scope,
    };
    const Builder = struct {
        scopeBuilders: ArrayList(Scope.Builder),
        scopes: ArrayList(Scope),
        currentScope: u32,
    };
};

pub const Parent = struct {
    expr: Entity,
    scope: Entity,
};
//TODO:make naming consistent
pub const Scope = struct {
    statements: []Entity,
    VarDeclaration: []VarDeclaration,
    Assignment: []Assignment,
    CompoundAssignment: []CompoundAssignment,
    Comparison: []Comparison,
    BreakExpr: []BreakExpr,
    ReturnExpr: []ReturnExpr,
    InvokeExpr: []InvokeExpr,
    IdentifierExpr: []IdentifierExpr,
    ArithmeticExpr: []ArithmeticExpr,
    ArraySubExpr: []ArraySubExpr,
    FieldAccessExpr: []FieldAccessExpr,
    Loop: []Loop,
    Branches: []Branch,
    Parent: Parent,

    pub const Builder = struct {
        Statements: ArrayList(Entity),
        VarDeclarations: ArrayList(VarDeclaration),
        Assignments: ArrayList(Assignment),
        CompoundAssignments: ArrayList(CompoundAssignment),
        Comparisons: ArrayList(Comparison),
        BreakExprs: ArrayList(BreakExpr),
        ReturnExprs: ArrayList(ReturnExpr),
        InvokeExprs: ArrayList(InvokeExpr),
        IdentifierExpr: ArrayList(IdentifierExpr),
        ArithmeticExprs: ArrayList(ArithmeticExpr),
        ArraySubExprs: ArrayList(ArraySubExpr),
        FieldAccessExpr: ArrayList(FieldAccessExpr),
        LastLoop: Entity,
        Loops: ArrayList(Loop),
        Branches: ArrayList(Branch),
        Parent: Parent,

        pub fn new(allocator: *Allocator, builder: *Function.Builder, parentExpr: Entity, parentScope: Entity) u32 {
            const last = if (builder.scopeBuilders.items.len == 0) std.mem.zeroes(Entity) else builder.scopeBuilders.items[builder.currentScope].LastLoop;

            builder.currentScope = @as(u32, @intCast(builder.scopeBuilders.items.len));
            builder.scopes.append(undefined) catch unreachable;
            builder.scopeBuilders.append(Scope.Builder{
                .Statements = ArrayList(Entity).init(allocator),
                .VarDeclarations = ArrayList(VarDeclaration).init(allocator),
                .Assignments = ArrayList(Assignment).init(allocator),
                .CompoundAssignments = ArrayList(CompoundAssignment).init(allocator),
                .Comparisons = ArrayList(Comparison).init(allocator),
                .BreakExprs = ArrayList(BreakExpr).init(allocator),
                .ReturnExprs = ArrayList(ReturnExpr).init(allocator),
                .InvokeExprs = ArrayList(InvokeExpr).init(allocator),
                .IdentifierExpr = ArrayList(IdentifierExpr).init(allocator),
                .ArithmeticExprs = ArrayList(ArithmeticExpr).init(allocator),
                .ArraySubExprs = ArrayList(ArraySubExpr).init(allocator),
                .FieldAccessExpr = ArrayList(FieldAccessExpr).init(allocator),
                .LastLoop = last,
                .Loops = ArrayList(Loop).init(allocator),
                .Branches = ArrayList(Branch).init(allocator),
                .Parent = .{ .expr = parentExpr, .scope = parentScope },
            }) catch unreachable;
            return builder.currentScope;
        }
        // Maybe this is a better way to initialize the builder
        // fn initScopeBuilder(allocator: *Allocator, lastLoop: ?*Loop) Scope.Builder {
        //     return Scope.Builder{
        //         .Statements = ArrayList(Entity).init(allocator),
        //         .VarDeclarations = ArrayList(VarDeclaration).init(allocator),
        //         .Assignments = ArrayList(Assignment).init(allocator),
        //         .CompoundAssignments = ArrayList(CompoundAssignment).init(allocator),
        //         .Comparisons = ArrayList(Comparison).init(allocator),
        //         .BreakExprs = ArrayList(BreakExpr).init(allocator),
        //         .ReturnExprs = ArrayList(ReturnExpr).init(allocator),
        //         .InvokeExprs = ArrayList(InvokeExpr).init(allocator),
        //         .ArithmeticExprs = ArrayList(ArithmeticExpr).init(allocator),
        //         .ArraySubExprs = ArrayList(ArraySubExpr).init(allocator),
        //         .FieldAccessExpr = ArrayList(FieldAccessExpr).init(allocator),
        //         .LastLoop = lastLoop,
        //         .Loops = ArrayList(Loop).init(allocator),
        //     };
        // }

        // fn new(allocator: *Allocator, builder: *Function.Builder) u32 {
        //     const lastLoop = if (builder.scopeBuilders.items.len == 0) null else builder.scopeBuilders.items[builder.currentScope].LastLoop;

        //     builder.currentScope = @as(u32, @as(u32, @intCast(builder.scopeBuilders.items.len)));
        //     builder.scopes.append(undefined) catch unreachable;
        //     builder.scopeBuilders.append(initScopeBuilder(allocator, lastLoop));
        //     return builder.currentScope;
        // }
    };
};

pub const Branch = struct {
    condition: Entity,
    ifScope: u32,
    elseScope: u32,
    exitBlk: IR.Ref,
};

pub const VarDeclaration = struct {
    name: []const u8,
    type: Type,

    fn new(fnBuilder: *Function.Builder, name: []const u8, varType: Type) Entity {
        const currentScope = &fnBuilder.scopeBuilders.items[fnBuilder.currentScope];

        const varDeclarationIdx = currentScope.VarDeclaration.items.len;
        currentScope.VarDeclarations.append(.{
            .name = name,
            .type = varType,
        }) catch unreachable;

        const varDeclarationId = Entity.new(varDeclarationIdx, Scopes.VarDeclaration, fnBuilder.currentScope);
        currentScope.Statements.append(varDeclarationId) catch unreachable;

        return varDeclarationId;
    }
};
pub const ReturnExpr = struct {
    expr: ?Entity,
};

pub const BreakExpr = struct {
    loop2Break: EntityID,
};

pub const ArithmeticExpr = struct {
    left: Entity,
    right: Entity,
    id: ID,
    const ID = enum(u8) {
        Add,
        Sub,
        Mul,
        Div,
        Mod,
    };
};

pub const InvokeExpr = struct {
    args: []Entity,
    expr: Entity,
    fn new(fnBuilder: *Function.Builder, expr: Entity, args: []Entity) Entity {
        const currentScope = &fnBuilder.scopeBuilders.items[fnBuilder.currentScope];

        const idx = currentScope.Statements.items.len;
        const invokeExpr = Entity.new(idx, Scopes.InvokeExpr, fnBuilder.currentScope);
        currentScope.InvokeExprs.append(.{
            .args = args,
            .expr = expr,
        }) catch unreachable;

        return invokeExpr;
    }
};
pub const IdentifierExpr = []const u8;
pub fn newIdentifierExpr(fnBuilder: *Function.Builder, name: IdentifierExpr) Entity {
    const currentScope = &fnBuilder.scopeBuilders.items[fnBuilder.currentScope];

    const idx = currentScope.IdentifierExpr.items.len;
    currentScope.IdentifierExpr.append(name) catch unreachable;

    return Entity.new(idx, Scopes.IdentifierExpr, fnBuilder.currentScope);
}
pub const Assignment = struct {
    left: Entity,
    right: Entity,
    fn new(fnBuilder: *Function.Builder, left: Entity, right: Entity) Entity {
        const currentScope = &fnBuilder.scopeBuilders.items[fnBuilder.currentScope];

        const idx = currentScope.Statements.items.len;

        const assignment = Entity.new(idx, Scopes.Assignment, fnBuilder.currentScope);
        currentScope.Assignments.append(.{
            .left = left,
            .right = right,
        }) catch unreachable;

        return assignment;
    }
};

pub const CompoundAssignment = struct {
    left: Entity,
    right: Entity,
    id: EntityID,

    fn new(fnBuilder: *Function.Builder, id: EntityID, left: Entity, right: Entity) Entity {
        const currentScope = &fnBuilder.scopeBuilders.items[fnBuilder.currentScope];

        const idx = currentScope.Statements.items.len;
        currentScope.CompoundAssignments.append(.{
            .left = left,
            .right = right,
            .id = id,
        }) catch unreachable;

        return Entity.new(idx, Scopes.CompoundAssignment, fnBuilder.currentScope);
    }
};

pub const Comparison = struct {
    left: Entity,
    right: Entity,
    id: ID,
    const ID = enum(u8) {
        Equal,
        NotEqual,
        LessThan,
        GreaterThan,
        LessThanOrEqual,
        GreaterThanOrEqual,
    };
    fn new(fnBuilder: *Function.Builder, id: ID, left: Entity, right: Entity) Entity {
        const currentScope = &fnBuilder.scopeBuilders.items[fnBuilder.currentScope];
        const idx = currentScope.Comparisons.items.len;
        const comparisonId = Entity.new(idx, Scopes.Comparison, fnBuilder.currentScope);
        currentScope.Comparisons.append(.{
            .left = left,
            .right = right,
            .id = id,
        }) catch unreachable;
        return comparisonId;
    }
};

pub const Loop = struct {
    prefixScopeIdx: u32,
    bodyScopeIdx: u32,
    postfixScopeId: u32,
    exitBlk: IR.Ref,
    continueBlk: IR.Ref,
};

pub const Lib = struct {
    symbolNames: [][]const u8,

    const Builder = struct {
        symbolNames: ArrayList([]const u8),
    };
};

pub const TokenTypeMap = blk: {
    var ttm: [std.enums.values(Lexer.Token).len]type = undefined;
    ttm[@intFromEnum(Token.IntLiteral)] = Lexer.IntLiteral;
    ttm[@intFromEnum(Token.CharLiteral)] = Lexer.CharLiteral;
    ttm[@intFromEnum(Token.StringLiteral)] = Lexer.StringLiteral;
    ttm[@intFromEnum(Token.Identifier)] = Lexer.Identifier;
    ttm[@intFromEnum(Token.Operator)] = Lexer.Operator;
    ttm[@intFromEnum(Token.Keyword)] = Lexer.Keyword;
    ttm[@intFromEnum(Token.Sign)] = Lexer.Sign;
    break :blk ttm;
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

pub fn main() !void {}
