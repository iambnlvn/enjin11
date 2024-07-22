const std = @import("std");
const ArrayList = std.ArrayList;

const Function = @import("Parser.zig").Function;
const Type = @import("Type.zig");
const Parser = @import("Parser.zig");
pub const Module = struct {
    internalFns: []Function.Internal,
    intLiterals: []Parser.IntegerLiteral,
    structLiterals: []Parser.StructLiteral,
    arrayLiterals: []Parser.ArrayLiteral,
    unresolvedTypes: []Type.UnresolvedType,
    sliceTypes: []Type.Slice,
    fnTypes: Type.Function,
    arrayTypes: []Type.Array,
    structTypes: []Type.Struct,
    libNames: []([]const u8),

    pub const Builder = struct {
        internalFns: ArrayList(Function.Internal),
        intLiterals: ArrayList(Parser.IntegerLiteral),
        structLiterals: ArrayList(Parser.StructLiteral),
        arrayLiterals: ArrayList(Parser.ArrayLiteral),
        unresolvedTypes: ArrayList(Type.UnresolvedType),
        sliceTypes: ArrayList(Type.Slice),
        fnTypes: ArrayList(Type.Function),
        arrayTypes: ArrayList(Type.Array),
        structTypes: ArrayList(Type.Struct),
        libNames: ArrayList([]const u8),
        idx: u32,
    };
};
