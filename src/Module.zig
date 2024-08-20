const std = @import("std");
const ArrayList = std.ArrayList;

const Function = @import("Parser.zig").Function;
const Type = @import("Type.zig");
const Parser = @import("Parser.zig");
pub const Module = struct {
    internalFns: []Function.Internal,
    externalFns: []Function.External,
    importedModules: []Parser.ImportedModule,
    intLiterals: []Parser.IntegerLiteral,
    structLiterals: []Parser.StructLiteral,
    arrayLiterals: []Parser.ArrayLiteral,
    unresolvedTypes: [][]const u8,
    sliceTypes: []Type.Slice,
    fnTypes: Type.Function,
    arrayTypes: []Type.Array,
    structTypes: []Type.Struct,
    pointerTypes: []Type.Pointer,
    libNames: []([]const u8),
    libs: []Parser.Lib.Builder,

    pub const Builder = struct {
        internalFns: ArrayList(Function.Internal),
        externalFns: ArrayList(Function.External),
        importedModules: ArrayList(Parser.ImportedModule),
        intLiterals: ArrayList(Parser.IntegerLiteral),
        structLiterals: ArrayList(Parser.StructLiteral),
        arrayLiterals: ArrayList(Parser.ArrayLiteral),
        unresolvedTypes: ArrayList([]const u8),
        pointerTypes: ArrayList(Type.Pointer),
        sliceTypes: ArrayList(Type.Slice),
        fnTypes: ArrayList(Type.Function),
        arrayTypes: ArrayList(Type.Array),
        structTypes: ArrayList(Type.Struct),
        libNames: ArrayList([]const u8),
        libs: ArrayList(Parser.Lib.Builder),
        idx: u32,
    };
};
