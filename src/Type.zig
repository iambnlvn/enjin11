const std = @import("std");

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

const Integer = struct {
    pub const Signedness = enum {
        Signed,
        Unsigned,
        const position = Resolution.position - @bitSizeOf(Signedness);
    };

    pub fn new(bitCount: u16, signedness: Signedness) Type {
        {
            return .{ .value = (@as(u64, @intFromEnum(Type.ID.integer)) << Type.ID.position) | (@as(u64, @intFromEnum(signedness)) << Signedness.position) | bitCount };
        }
    }
};
