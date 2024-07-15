const std = @import("std");
const LLVM = @import("LLVM.zig");

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

        // Todo: Implement Fields
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
