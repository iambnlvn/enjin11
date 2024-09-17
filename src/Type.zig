const std = @import("std");
const expectEqual = std.testing.expectEqual;
const ArrayList = std.ArrayList;
const Ir = @import("IR.zig");
const Formatter = @import("Formatter.zig");
pub const Type = @This();
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
    pub const Signedness = enum {
        Signed,
        Unsigned,
        const position = Resolution.position - SignednessBitSize;
    };
    const SignednessBitSize = @bitSizeOf(Signedness);
    pub fn new(bitCount: u16, signedness: Signedness) Type {
        {
            return .{ .value = (@as(u64, @intFromEnum(Type.ID.Integer)) << Type.ID.position) | (@as(u64, @intFromEnum(signedness)) << Signedness.position) | bitCount };
        }
    }
    pub fn getBitCount(T: Type) u16 {
        return @as(u16, @truncate(T.value));
    }
    // this implementation is less accurate than the one below for extracting the signedness info
    // esp if the Signedness enum could occupy more than 1 bit (the new implementation accounts for the bit size of Signedness)
    // pub fn getSignedness(T: Type) Signedness {
    //     return @as(Signedness, @enumFromInt(@as(u1, @intCast((T.value & (1 << Signedness.position)) >> Signedness.position))));
    // }

    pub fn getSignedness(T: Type) Signedness {
        return @as(Signedness, @enumFromInt(@as(u64, (T.value >> Signedness.position) & ((1 << SignednessBitSize) - 1))));
    }
};

pub const Boolean = Integer.new(1, .Unsigned);

pub const Function = struct {
    argTypes: []Type,
    returnType: Type,
    attributes: u64,

    pub const Attribute = enum(u64) {
        noreturn,
        @"extern",
    };

    pub fn new(idx: u64, moduleIdx: u64) Type {
        return constructTypeValue(Type.ID.Function, moduleIdx, idx);
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

    pub fn new(idx: u64, moduleIdx: u64) Type {
        return constructTypeValue(Type.ID.Array, moduleIdx, idx);
    }
};

pub const Slice = struct {
    type: Type,
};

pub const Struct = struct {
    types: []Type,
    names: [][]const u8,
    name: []const u8,
    alignment: u64,

    pub fn new(idx: u64, moduleIdx: u64) Type {
        return constructTypeValue(Type.ID.Structure, moduleIdx, idx);
    }
};

pub const Pointer = struct {
    type: Type,
    pub const size = 8;
    pub fn new(idx: u64, moduleIdx: u64) Type {
        return constructTypeValue(Type.ID.Pointer, moduleIdx, idx);
    }

    pub fn getType(self: Type, pointerTypes: []Type.Pointer) Type {
        return pointerTypes[@as(u32, @truncate(self.value))].type;
    }

    pub fn getBaseType(self: Type, ptrTypes: []Type.Pointer) Type {
        return ptrTypes[self.getIdx()].type;
    }
    pub fn getPointeeType(self: Type, ptrTypes: []Type.Pointer) Type {
        return ptrTypes[self.getIdx()].type;
    }
};

pub const Builtin = struct {
    const ID = enum {
        voidType,
        noreturnType,
    };
    pub const voidType = Type{ .value = (@as(u64, @intFromEnum(Type.ID.Builtin)) << Type.ID.position) | @as(u64, @intFromEnum(Builtin.ID.voidType)) };
    pub const noreturnType = Type{ .value = (@as(u64, @intFromEnum(Type.ID.Builtin)) << Type.ID.position) | @as(u64, @intFromEnum(Builtin.ID.noreturnType)) };
};

fn constructTypeValue(id: Type.ID, moduleIdx: u64, idx: u64) Type {
    return .{ .value = (@as(u64, @intFromEnum(id)) << Type.ID.position) | (moduleIdx << Module.position) | idx };
}
pub fn newUnresolvedType(idx: u64, moduleIdx: u64) Type {
    return constructTypeValue(Type.ID.Pointer, moduleIdx, idx);
}

pub fn isResolved(self: Type) bool {
    return (self.value & (1 << Type.Resolution.position)) >> Type.Resolution.position != 0;
}

pub fn markResolved(self: *Type) void {
    self.value |= 1 << Type.Resolution.position;
}

pub fn getId(self: Type) ID {
    return @as(ID, @enumFromInt(@as(u4, @intCast((self.value & (std.math.maxInt(u4) << ID.position)) >> ID.position))));
}

pub fn getIdx(self: Type) u32 {
    return @as(u32, @truncate(self.value));
}

pub fn setNewIdx(self: *Type, newIdx: u64) void {
    self.value = (self.value & 0xffffffff00000000) | newIdx;
}

pub fn getModuleIdx(self: Type) u64 {
    return (self.value & (Module.mask << Module.position)) >> Module.position;
}

pub fn getSizeResolved(self: Type, program: *const Ir.Program) u64 {
    switch (self.getId()) {
        .Integer => return Integer.getBitCount(self) >> 3,
        .Pointer => return Pointer.size,
        .Array => {
            const arrayType = &program.arrayTypes[self.get_index()];
            return arrayType.type.getSizeResolved(program) * arrayType.exprLen;
        },
        .Structure => {
            var structSize: 32 = 0;
            const structType = &program.structTypes[self.get_index()];
            for (structType.types) |t| {
                structSize += @as(u32, t.getSizeResolved(program));
            }
            return structSize;
        },
        else => std.debug.panic("getSizeResolved not implemented for type"),
    }
}

pub fn getSize(self: Type, program: *const Ir.Program.Builder) u64 {
    switch (self.getId()) {
        .Integer => return Integer.getBitCount(self) >> 3,
        .Pointer => return Pointer.size,
        .Array => {
            const arrayType = &program.arrayTypes[self.get_index()];
            return arrayType.type.getSizeResolved(program) * arrayType.exprLen;
        },
        .Structure => {
            var structSize: 32 = 0;
            const structType = &program.structTypes[self.get_index()];
            for (structType.types) |t| {
                structSize += @as(u32, t.getSizeResolved(program));
            }
            return structSize;
        },
        else => std.debug.panic("getSizeResolved not implemented for type"),
    }
}

pub fn toString(self: Type, formatter: *const Formatter) []const u8 {
    switch (self.getId()) {
        .Builtin => {
            if (self.value == Type.Builtin.voidType.value) {
                return "void";
            } else if (self.value == Type.Builtin.noreturnType.value) {
                return "noreturn";
            } else unreachable;
        },
        .Integer => {
            const bitCount = Integer.getBitCount(self);
            return switch (Integer.getSignedness(self)) {
                .Signed => std.fmt.allocPrint(formatter.allocator, "s{}", .{bitCount}) catch unreachable,
                .Unsigned => std.fmt.allocPrint(formatter.allocator, "u{}", .{bitCount}) catch unreachable,
            };
        },
        .Pointer => {
            const ptrType = &formatter.builder.pointerTypes.items[self.get_index()];
            return .fmt.allocPrint(formatter.allocator, "s{}*", .{ptrType.type.toString(formatter)}) catch unreachable;
        },
        .Array => {
            const arrayType = &formatter.builder.ArrayTypes.items[self.get_index()];
            return .fmt.allocPrint(formatter.allocator, "[{} x {s}]", .{ arrayType.exprLen, arrayType.type.toString(formatter) }) catch unreachable;
        },
        .Structure => {
            const structType = &formatter.builder.structTypes.items[self.get_index()];
            return structType.name;
        },
        else => std.debug.panic("toString not implemented for type"),
    }
}

test "Integer.new initializes correctly" {
    const bitCount: u16 = 64;
    const signedness = Type.Integer.Signedness.Unsigned;
    const expectedValue: u64 = (@as(u64, @intFromEnum(Type.ID.Integer)) << Type.ID.position) | (@as(u64, @intFromEnum(signedness)) << Type.Integer.Signedness.position) | bitCount;

    const integer = Type.Integer.new(bitCount, signedness);

    try expectEqual(expectedValue, integer.value);
}
test "Integer.getBitCount returns correct value" {
    const bitCount: u16 = 64;
    const signedness = Type.Integer.Signedness.Unsigned;
    const integer = Type.Integer.new(bitCount, signedness);

    try expectEqual(bitCount, Type.Integer.getBitCount(integer));
}

test "Integer.getSignedness2 returns correct value" {
    const bitCount: u16 = 64;
    const signedness = Type.Integer.Signedness.Unsigned;
    const integer = Type.Integer.new(bitCount, signedness);

    try expectEqual(signedness, Type.Integer.getSignedness(integer));
}

test "Boolean initializes correctly" {
    const expectedValue: u64 = (@as(u64, @intFromEnum(Type.ID.Integer)) << Type.ID.position) | (1 << Type.Integer.Signedness.position) | 1;

    try expectEqual(expectedValue, Type.Boolean.value);
}
test "Boolean is unsigned" {
    try expectEqual(Type.Integer.Signedness.Unsigned, Type.Integer.getSignedness(Type.Boolean));
}
test "Boolean is 1 bit" {
    try expectEqual(1, Type.Integer.getBitCount(Type.Boolean));
}
//Todo!: Fix this test
// test "Function.new initializes correctly" {
//     const idx: u64 = 1;
//     const moduleIdx: u64 = 2;
//     const expectedValue: u64 = (@as(u64, @intFromEnum(Type.ID.Function)) << Type.ID.position) | (moduleIdx << Type.Module.position) | idx;

//     const function = Type.Function.new(idx, moduleIdx);

//     try expectEqual(expectedValue, function.value);
// }
// test "Function.append appends correctly" {
//     var fnTypes = ArrayList(Type.Function).init(std.testing.allocator);
//     defer fnTypes.deinit();

//     const idx: u64 = 1;
//     const moduleIdx: u64 = 2;
//     const expectedValue: u64 = (@as(u64, @intFromEnum(Type.ID.Function)) << Type.ID.position) | (moduleIdx << Type.Module.position) | idx;

//     const function = Type.Function.new(idx, moduleIdx);
//     const appendedFunction = Type.Function.append(&fnTypes, function, moduleIdx);

//     try expectEqual(expectedValue, appendedFunction.value);
//     try expectEqual(1, fnTypes.items.len);
// }

test "Array.new initializes correctly" {
    const idx: u64 = 1;
    const moduleIdx: u64 = 2;
    const expectedValue: u64 = (@as(u64, @intFromEnum(Type.ID.Array)) << Type.ID.position) | (moduleIdx << Type.Module.position) | idx;

    const array = Type.Array.new(idx, moduleIdx);

    try expectEqual(expectedValue, array.value);
}

test "Struct.new initializes correctly" {
    const idx: u64 = 1;
    const moduleIdx: u64 = 2;
    const expectedValue: u64 = (@as(u64, @intFromEnum(Type.ID.Structure)) << Type.ID.position) | (moduleIdx << Type.Module.position) | idx;

    const s = Type.Struct.new(idx, moduleIdx);

    try expectEqual(expectedValue, s.value);
}

test "Pointer.new initializes correctly" {
    const moduleIdx: u64 = 1;
    const idx: u64 = 2;
    const expectedValue: u64 = (@as(u64, @intFromEnum(Type.ID.Pointer)) << Type.ID.position) | (moduleIdx << Type.Module.position) | idx;

    const pointer = Type.Pointer.new(idx, moduleIdx);

    try expectEqual(expectedValue, pointer.value);
    try expectEqual(8, Type.Pointer.size);
}

test "Pointer.getType returns correct type" {
    var pointerTypes = [_]Type.Pointer{.{ .type = Type.Boolean }};
    const pointer = Type.Pointer.new(0, 0);

    try expectEqual(Type.Boolean, Type.Pointer.getType(pointer, &pointerTypes));
}
test "isResolved returns false for unresolved type" {
    const unresolvedType = Type.newUnresolvedType(0, 0);

    try expectEqual(false, Type.isResolved(unresolvedType));
}
test "isResolved returns true for resolved type" {
    var resolvedType = Type.newUnresolvedType(0, 0);
    Type.markResolved(&resolvedType);

    try expectEqual(true, Type.isResolved(resolvedType));
}
test "getId returns correct ID" {
    const id = Type.ID.Integer;
    const integer = Type.Integer.new(64, Type.Integer.Signedness.Unsigned);

    try expectEqual(id, Type.getId(integer));
}
//Todo!: Fix this test or figure out how to get the correct value
// test "getModuleIdx returns correct module index" {
//     const moduleIdx: u64 = 1; returns 67108864
//     const integer = Type.Integer.new(64, Type.Integer.Signedness.Unsigned);

//     try expectEqual(moduleIdx, Type.getModuleIdx(integer));
// }

test "UnresolvedType initializes correctly" {
    const expectedValue: u64 = (@as(u64, @intFromEnum(Type.ID.Unresolved)) << Type.ID.position);

    try expectEqual(expectedValue, Type.UnresolvedType.value);
}

test "UnresolvedType is unresolved" {
    try expectEqual(false, Type.isResolved(Type.UnresolvedType));
}
test "UnresolvedType is resolved" {
    var resolvedType = Type.newUnresolvedType(0, 0);
    Type.markResolved(&resolvedType);

    try expectEqual(true, Type.isResolved(resolvedType));
}
test "UnresolvedType has correct ID" {
    try expectEqual(Type.ID.Unresolved, Type.getId(Type.UnresolvedType));
}

test "UnresolvedType has correct module index" {
    const moduleIdx: u64 = 1;
    const unresolvedType = Type.newUnresolvedType(0, moduleIdx);

    try expectEqual(moduleIdx, Type.getModuleIdx(unresolvedType));
}
