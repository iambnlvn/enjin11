const std = @import("std");
const LLVM = @import("LLVM.zig");
const Type = @import("Type.zig");
const Parser = @import("Parser.zig");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
pub const Constant = struct {
    pub const ID = enum(u8) {
        Array,
        @"struct",
        vector,
        AggregateZero,
        DataArray,
        DataVector,
        Fp,
        Int,
        Np,

        const position = Ref.ID.position - @bitSizeOf(ID);
    };

    fn new(id: ID, index: u32) Ref {
        return .{ .value = (@as(u64, @intFromEnum(Ref.ID.constant)) << Ref.ID.position) | (@as(u64, @intFromEnum(id)) << ID.position) | index };
    }

    pub fn getId(reference: Ref) ID {
        return @as(ID, @enumFromInt(@as(u8, @intCast((reference.value & (std.math.maxInt(std.meta.Int(.unsigned, @bitSizeOf(ID))) << ID.position)) >> ID.position))));
    }

    const Struct = struct {
        fields: []Type,
        FieldValues: [][]const u8,
        FieldNames: [][]const u8,
    };
};

pub const Ref = struct {
    const T = u64;
    value: T,

    pub const Null = std.mem.zeroes(T);

    pub const ID = enum(u8) {
        none,
        Module,
        Arg,
        BasicBlock,
        Constant,
        GlobalFunc,
        GlobalVar,
        Instruction,
        Operator,
        ExternalFunc,

        const position = @bitSizeOf(T) - @bitSizeOf(ID);
    };
    const LLVMID = LLVM.ID;

    fn getId(self: Ref) ID {
        return @as(ID, @enumFromInt(@as(u8, @intCast((self.value & (std.math.maxInt(std.meta.Int(.unsigned, @bitSizeOf(ID))) << ID.position)) >> ID.position))));
    }

    fn getIDX(self: Ref) u64 {
        return @as(u64, @truncate(self.value));
    }
};

pub const Function = struct {
    declaration: Parser.Function,
    basicBlocs: u32,
    argAlloca: []Ref,

    fn newFunction(idx: u32) Ref {
        return .{ .value = (@as(u64, @intFromEnum(Ref.ID.GlobalFunc)) << Ref.ID.position) | idx };
    }

    const Builder = struct {
        declaration: Parser.Function,
        basicBlocs: ArrayList(u32),
        argAlloca: ArrayList(Ref),
        returnAlloca: Ref,
        currentBlock: u32,
        nextAllocationIdx: u32,
        exitBlock: u32,
        scope2BasicBlockMap: ArrayList(u32),
        isExplicitReturn: bool,
        isEmittedReturn: bool,
        isConditionalAlloca: bool,

        fn new(allocator: *Allocator, function: *Parser.Function.Internal) Function.Builder {
            var builder = Builder{
                .declaration = function.declaration,
                .basicBlocs = ArrayList(u32).init(allocator),
                .argAlloca = ArrayList(Ref).initCapacity(allocator, function.declaration.argNames.len) catch unreachable,
                .returnAlloca = Ref.Null,
                .currentBlock = 0,
                .nextAllocationIdx = 0,
                .exitBlock = 0,
                .scope2BasicBlockMap = ArrayList(u32).initCapacity(allocator, function.scopes.len) catch unreachable,
                .isExplicitReturn = false,
                .isEmittedReturn = false,
                .isConditionalAlloca = false,
            };
            builder.scope2BasicBlockMap.items.len = function.scopes.len;
            return builder;
        }
    };
    pub const Arg = struct {
        pub fn new(idx: u32) Ref {
            return .{ .value = (@as(u64, @intFromEnum(Ref.ID.Arg)) << Ref.ID.position) | idx };
        }
    };

    pub const VarDeclaration = struct {
        fn new(idx: u32) Ref {
            return .{ .value = (@as(u64, @intFromEnum(Ref.ID.GlobalVar)) << Ref.ID.position) | idx };
        }
    };
};

pub const ExternalFunc = struct {
    libName: []const u8,
    type: Type,

    fn new(idx: u32) Ref {
        return .{ .value = (@as(u64, @intFromEnum(Ref.ID.ExternalFunc)) << Ref.ID.position) | idx };
    }
};
