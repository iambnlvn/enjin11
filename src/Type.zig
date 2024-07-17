const std = @import("std");
const ArrayList = std.ArrayList;
const Type = @This();
value: u64,

pub const UnresolvedType = Type{ .value = 0 };

pub const ID = enum(u4) {
    Unresolved,
    Builtin,
    Integer,
    Pointer,
    Structure,
    Array,
    Slice,
    Function,

    const position = @bitSizeOf(Type) - @bitSizeOf(ID);
};

const Resolution = struct {
    const position = Type.ID.position - @bitSizeOf(u1);
};

const Module = struct {
    const position = @bitSizeOf(Type) / 2;
    const bitCount = Resolution.position - Module.position;
    const mask = std.math.maxInt(std.meta.Int(.unsigned, bitCount));
};

pub const Integer = struct {
    const Signedness = enum {
        Signed,
        Unsigned,
        const position = Resolution.position - @bitSizeOf(Signedness);
    };

    pub fn new(bitCount: u16, signedness: Signedness) Type {
        {
            return .{ .value = (@as(u64, @intFromEnum(Type.ID.integer)) << Type.ID.position) | (@as(u64, @intFromEnum(signedness)) << Signedness.position) | bitCount };
        }
    }
    pub fn getBitCount(T: Type) u16 {
        return @as(u16, @truncate(T.value));
    }
    pub fn getSignedness(T: Type) Signedness {
        return @as(Signedness, @enumFromInt(@as(u1, @intCast((T.value & (1 << Signedness.position)) >> Signedness.position))));
    }
};

pub const Boolean = Integer.new(1, .Unsigned);

pub const Function = struct {
    argTypes: []Type,
    returnType: Type,
    attributes: u64,

    const Attribute = enum(u64) {
        noreturn,
    };

    pub fn new(idx: u64, module: u64) Type {
        return .{ .value = (@as(u64, @intFromEnum(Type.ID.Function)) << Type.ID.position) | (module << Module.position) | idx };
    }

    pub fn append(fnTypes: *ArrayList(Type.Function), fnType: Type.Function, moduleIdx: u64) Type {
        const idx = fnTypes.items.len;
        fnTypes.append(fnType) catch unreachable;
        return new(idx, moduleIdx);
    }
};

pub const Array = struct {
    exprLen: u64,
    type: Type,

    fn new(idx: u64, moduleIdx: u64) Type {
        return .{ .value = (@as(u64, @intFromEnum(Type.ID.array)) << Type.ID.position) | (moduleIdx << Module.position) | idx };
    }
};
