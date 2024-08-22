const Parser = @import("Parser.zig");
const Type = @import("Type.zig");
pub const Result = struct {
    functions: []Parser.Function.Internal,
    external: External,
    importedModules: []Parser.ImportedModule,

    integerLiterals: []Parser.IntegerLiteral,
    arrayLiterals: []Parser.ArrayLiteral,
    structLiterals: []Parser.StructLiteral,

    sliceTypes: []Type.Slice,
    functionTypes: []Type.Function,
    arrayTypes: []Type.Array,
    structTypes: []Type.Struct,
    pointerTypes: []Type.Pointer,
};

pub const External = struct {
    functions: []Parser.Function.External,
    libNames: [][]const u8,
    symNames: [][]const u8,
    libs: []Library,

    pub const Library = struct {
        symbols: []u32,
    };
};
