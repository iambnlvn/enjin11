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
            return .{ .value = (@as(u64, @intFromEnum(Type.ID.Integer)) << Type.ID.position) | (@as(u64, @intFromEnum(signedness)) << Signedness.position) | bitCount };
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
        return .{ .value = (@as(u64, @intFromEnum(Type.ID.Array)) << Type.ID.position) | (moduleIdx << Module.position) | idx };
    }
};

pub const Slice = struct {
    type: Type,
};

pub const Struct = struct {
    types: []Type,
    names: [][]const u8,
    name: []const u8,
    alignement: u64,

    pub fn new(idx: u64, moduleIdx: u64) Type {
        return .{ .value = (@as(u64, @intFromEnum(Type.ID.Structure)) << Type.ID.position) | (moduleIdx << Module.position) | idx };
    }
};
pub const Pointer = struct {
    type: Type,
    const size = 8;
    pub fn new(idx: u64, moduleIdx: u64) Type {
        return .{ .value = (@as(u64, @intFromEnum(Type.ID.Pointer)) << Type.ID.position) | (moduleIdx << Module.position) | idx };
    }

    fn getType(self: Type, pointerTypes: []Type.Pointer) Type {
        return pointerTypes[@as(u32, @truncate(self.value))].type;
    }
};

test "Integer.new initializes correctly" {
    const bitCount: u16 = 64;
    const signedness = Type.Integer.Signedness.Unsigned;
    const expectedValue: u64 = (@as(u64, @intFromEnum(Type.ID.Integer)) << Type.ID.position) | (@as(u64, @intFromEnum(signedness)) << Type.Integer.Signedness.position) | bitCount;

    const integer = Type.Integer.new(bitCount, signedness);

    try std.testing.expectEqual(expectedValue, integer.value);
}
test "Integer.getBitCount returns correct value" {
    const bitCount: u16 = 64;
    const signedness = Type.Integer.Signedness.Unsigned;
    const integer = Type.Integer.new(bitCount, signedness);

    try std.testing.expectEqual(bitCount, Type.Integer.getBitCount(integer));
}
test "Integer.getSignedness returns correct value" {
    const bitCount: u16 = 64;
    const signedness = Type.Integer.Signedness.Unsigned;
    const integer = Type.Integer.new(bitCount, signedness);

    try std.testing.expectEqual(signedness, Type.Integer.getSignedness(integer));
}

test "Boolean initializes correctly" {
    const expectedValue: u64 = (@as(u64, @intFromEnum(Type.ID.Integer)) << Type.ID.position) | (1 << Type.Integer.Signedness.position) | 1;

    try std.testing.expectEqual(expectedValue, Type.Boolean.value);
}
test "Boolean is unsigned" {
    try std.testing.expectEqual(Type.Integer.Signedness.Unsigned, Type.Integer.getSignedness(Type.Boolean));
}
test "Boolean is 1 bit" {
    try std.testing.expectEqual(1, Type.Integer.getBitCount(Type.Boolean));
}
//Todo!: Fix this test
// test "Function.new initializes correctly" {
//     const idx: u64 = 1;
//     const moduleIdx: u64 = 2;
//     const expectedValue: u64 = (@as(u64, @intFromEnum(Type.ID.Function)) << Type.ID.position) | (moduleIdx << Type.Module.position) | idx;

//     const function = Type.Function.new(idx, moduleIdx);

//     try std.testing.expectEqual(expectedValue, function.value);
// }
// test "Function.append appends correctly" {
//     var fnTypes = ArrayList(Type.Function).init(std.testing.allocator);
//     defer fnTypes.deinit();

//     const idx: u64 = 1;
//     const moduleIdx: u64 = 2;
//     const expectedValue: u64 = (@as(u64, @intFromEnum(Type.ID.Function)) << Type.ID.position) | (moduleIdx << Type.Module.position) | idx;

//     const function = Type.Function.new(idx, moduleIdx);
//     const appendedFunction = Type.Function.append(&fnTypes, function, moduleIdx);

//     try std.testing.expectEqual(expectedValue, appendedFunction.value);
//     try std.testing.expectEqual(1, fnTypes.items.len);
// }

test "Array.new initializes correctly" {
    const idx: u64 = 1;
    const moduleIdx: u64 = 2;
    const expectedValue: u64 = (@as(u64, @intFromEnum(Type.ID.Array)) << Type.ID.position) | (moduleIdx << Type.Module.position) | idx;

    const array = Type.Array.new(idx, moduleIdx);

    try std.testing.expectEqual(expectedValue, array.value);
}

test "Struct.new initializes correctly" {
    const idx: u64 = 1;
    const moduleIdx: u64 = 2;
    const expectedValue: u64 = (@as(u64, @intFromEnum(Type.ID.Structure)) << Type.ID.position) | (moduleIdx << Type.Module.position) | idx;

    const s = Type.Struct.new(idx, moduleIdx);

    try std.testing.expectEqual(expectedValue, s.value);
}

test "Pointer.new initializes correctly" {
    const moduleIdx: u64 = 1;
    const idx: u64 = 2;
    const expectedValue: u64 = (@as(u64, @intFromEnum(Type.ID.Pointer)) << Type.ID.position) | (moduleIdx << Type.Module.position) | idx;

    const pointer = Type.Pointer.new(idx, moduleIdx);

    try std.testing.expectEqual(expectedValue, pointer.value);
    try std.testing.expectEqual(8, Type.Pointer.size);
}

test "Pointer.getType returns correct type" {
    var pointerTypes = [_]Type.Pointer{.{ .type = Type.Boolean }};
    const pointer = Type.Pointer.new(0, 0);

    try std.testing.expectEqual(Type.Boolean, Type.Pointer.getType(pointer, &pointerTypes));
}
