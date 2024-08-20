const std = @import("std");
const ArrayList = std.ArrayList;
const Parser = @import("Parser.zig");
const Type = @import("Type.zig");
const Sem = @import("Sem.zig");
const expect = std.testing.expect;
const Allocator = std.mem.Allocator;
const AST = @import("AST.zig").AST;
pub const resolutionBitsetSize = 64;

pub const Analyzer = struct {
    functions: ArrayList(Parser.Function.Internal),
    externalFunctions: ArrayList(Parser.Function.External),
    externalLibNames: ArrayList([]const u8),
    externalSymbolNames: ArrayList([]const u8),
    externalLibs: ArrayList(Sem.External.Library),
    importedModules: ArrayList(Parser.ImportedModule),
    integerLiterals: ArrayList(Parser.IntegerLiteral),
    arrayLiterals: ArrayList(Parser.ArrayLiteral),
    structLiterals: ArrayList(Parser.StructLiteral),
    pointerTypes: ArrayList(Type.Pointer),
    sliceTypes: ArrayList(Type.Slice),
    functionTypes: ArrayList(Type.Function),
    arrayTypes: ArrayList(Type.Array),
    structTypes: ArrayList(Type.Struct),
    unresolvedTypes: ArrayList([]const u8),
    arrayTypeResolutionBitsets: ArrayList([resolutionBitsetSize]u1),
    structTypeResolutionBitsets: ArrayList([resolutionBitsetSize]u1),
    moduleOffset: []ModuleStats,

    // Todo:  these methods can be refactored to something like this (javascripty style, not sure if it works in Zig)
    // fn isTypeResolved(bitsets: [][]u8, typeIdx: u64, resolutionBitsetSize: u64) bool {
    //     const bitsetIdx = typeIdx / resolutionBitsetSize;
    //     return bitsets[bitsetIdx][typeIdx % resolutionBitsetSize] == 1;
    // }

    // fn setTypeResolved(bitsets: [][]u8, typeIdx: u64, resolutionBitsetSize: u64) void {
    //     const bitsetIdx = typeIdx / resolutionBitsetSize;
    //     bitsets[bitsetIdx][typeIdx % resolutionBitsetSize] = 1;
    // }

    // fn isArrayTypeResolved(self: *Analyzer, resolvedArrayTypeIdx: u64) bool {
    //     return isTypeResolved(self.arrayTypeResolutionBitsets.items, resolvedArrayTypeIdx, resolutionBitsetSize);
    // }

    // fn setArrayTypeResolved(self: *Analyzer, resolvedArrayTypeIdx: u64) void {
    //     setTypeResolved(self.arrayTypeResolutionBitsets.items, resolvedArrayTypeIdx, resolutionBitsetSize);
    // }

    // fn isStructTypeResolved(self: *Analyzer, resolvedStructTypeIdx: u64) bool {
    //     return isTypeResolved(self.structTypeResolutionBitsets.items, resolvedStructTypeIdx, resolutionBitsetSize);
    // }

    // fn setStructTypeResolved(self: *Analyzer, resolvedStructTypeIdx: u64) void {
    //     setTypeResolved(self.structTypeResolutionBitsets.items, resolvedStructTypeIdx, resolutionBitsetSize);
    // }

    fn isArrayTypeResolved(self: *Analyzer, resolvedArrayTypeIdx: u64) bool {
        const bitsetIdx = resolvedArrayTypeIdx / resolutionBitsetSize;
        return self.arrayTypeResolutionBitsets.items[bitsetIdx][resolvedArrayTypeIdx % resolutionBitsetSize] == 1;
    }

    fn setArrayTypeResolved(self: *Analyzer, resolvedArrayTypeIdx: u64) void {
        const bitsetIdx = resolvedArrayTypeIdx / resolutionBitsetSize;
        self.arrayTypeResolutionBitsets.items[bitsetIdx][resolvedArrayTypeIdx % resolutionBitsetSize] = 1;
    }

    fn isStructTypeResolved(self: *Analyzer, resolvedStructTypeIdx: u64) bool {
        const bitsetIdx = resolvedStructTypeIdx / resolutionBitsetSize;
        return self.structTypeResolutionBitsets.items[bitsetIdx][resolvedStructTypeIdx % resolutionBitsetSize] == 1;
    }

    fn setStructTypeResolved(self: *Analyzer, resolvedStructTypeIdx: u64) void {
        const bitsetIdx = resolvedStructTypeIdx / resolutionBitsetSize;
        self.structTypeResolutionBitsets.items[bitsetIdx][resolvedStructTypeIdx % resolutionBitsetSize] = 1;
    }
};

pub const ModuleStats = struct {
    counters: [std.enums.values(ID).len]u64,

    const ID = enum {
        internalFns,
        externalFns,
        importedModules,
        integerLiterals,
        arrayLiterals,
        structLiterals,
        unresolvedTypes,
        pointerTypes,
        sliceTypes,
        functionTypes,
        arrayTypes,
        structTypes,
    };

    const Map = blk: {
        var arr: [std.enums.values(ID).len]type = undefined;
        for (std.enums.values(ID), 0..) |enumValue, idx| {
            arr[idx] = switch (enumValue) {
                .internalFns => Parser.Function.Internal,
                .externalFns => Parser.Function.External,
                .importedModules => Parser.ImportedModule,
                .integerLiterals => Parser.IntegerLiteral,
                .arrayLiterals => Parser.ArrayLiteral,
                .structLiterals => Parser.StructLiteral,
                .unresolvedTypes => []const u8,
                .pointerTypes => Type.Pointer,
                .sliceTypes => Type.Slice,
                .functionTypes => Type.Function,
                .arrayTypes => Type.Array,
                .structTypes => Type.Struct,
            };
        }
        break :blk arr;
    };
};

pub fn analyze(allocator: *Allocator, ast: AST) Sem.Result {
    const moduleArray = ast.modules[0..ast.moduleLen];
    var total = std.mem.zeroes(ModuleStats);
    var moduleOffsets = ArrayList(ModuleStats).initCapacity(allocator, ast.moduleLen) catch unreachable;
    moduleOffsets.resize(ast.moduleLen) catch unreachable;
    for (moduleArray, 0..) |module, moduleIdx| {
        moduleOffsets.items[moduleIdx].counters[@intFromEnum(ModuleStats.ID.internalFns)] = total.counters[@intFromEnum(ModuleStats.ID.internalFns)];
        total.counters[@intFromEnum(ModuleStats.ID.internalFns)] += module.internalFns.len;

        moduleOffsets.items[moduleIdx].counters[@intFromEnum(ModuleStats.ID.externalFns)] = total.counters[@intFromEnum(ModuleStats.ID.externalFns)];
        total.counters[@intFromEnum(ModuleStats.ID.externalFns)] += module.externalFns.len;

        moduleOffsets.items[moduleIdx].counters[@intFromEnum(ModuleStats.ID.importedModules)] = total.counters[@intFromEnum(ModuleStats.ID.importedModules)];
        total.counters[@intFromEnum(ModuleStats.ID.importedModules)] += module.importedModules.len;

        moduleOffsets.items[moduleIdx].counters[@intFromEnum(ModuleStats.ID.integerLiterals)] = total.counters[@intFromEnum(ModuleStats.ID.integerLiterals)];
        total.counters[@intFromEnum(ModuleStats.ID.integerLiterals)] += module.intLiterals.len;

        moduleOffsets.items[moduleIdx].counters[@intFromEnum(ModuleStats.ID.structTypes)] = total.counters[@intFromEnum(ModuleStats.ID.structTypes)];
        total.counters[@intFromEnum(ModuleStats.ID.structTypes)] += module.structTypes.len;

        moduleOffsets.items[moduleIdx].counters[@intFromEnum(ModuleStats.ID.arrayLiterals)] = total.counters[@intFromEnum(ModuleStats.ID.arrayLiterals)];
        total.counters[@intFromEnum(ModuleStats.ID.arrayLiterals)] += module.arrayLiterals.len;

        moduleOffsets.items[moduleIdx].counters[@intFromEnum(ModuleStats.ID.structLiterals)] = total.counters[@intFromEnum(ModuleStats.ID.structLiterals)];
        total.counters[@intFromEnum(ModuleStats.ID.structLiterals)] += module.structLiterals.len;

        moduleOffsets.items[moduleIdx].counters[@intFromEnum(ModuleStats.ID.arrayTypes)] = total.counters[@intFromEnum(ModuleStats.ID.arrayTypes)];
        total.counters[@intFromEnum(ModuleStats.ID.arrayTypes)] += module.arrayTypes.len;

        moduleOffsets.items[moduleIdx].counters[@intFromEnum(ModuleStats.ID.unresolvedTypes)] = total.counters[@intFromEnum(ModuleStats.ID.unresolvedTypes)];
        total.counters[@intFromEnum(ModuleStats.ID.unresolvedTypes)] += module.unresolvedTypes.len;

        moduleOffsets.items[moduleIdx].counters[@intFromEnum(ModuleStats.ID.sliceTypes)] = total.counters[@intFromEnum(ModuleStats.ID.sliceTypes)];
        total.counters[@intFromEnum(ModuleStats.ID.sliceTypes)] += module.sliceTypes.len;

        moduleOffsets.items[moduleIdx].counters[@intFromEnum(ModuleStats.ID.functionTypes)] = total.counters[@intFromEnum(ModuleStats.ID.functionTypes)];
        total.counters[@intFromEnum(ModuleStats.ID.functionTypes)] += module.fnTypes.len;

        moduleOffsets.items[moduleIdx].counters[@intFromEnum(ModuleStats.ID.arrayTypes)] = total.counters[@intFromEnum(ModuleStats.ID.arrayTypes)];
        total.counters[@intFromEnum(ModuleStats.ID.arrayTypes)] += module.arrayTypes.len;

        moduleOffsets.items[moduleIdx].counters[@intFromEnum(ModuleStats.ID.structTypes)] = total.counters[@intFromEnum(ModuleStats.ID.structTypes)];
        total.counters[@intFromEnum(ModuleStats.ID.structTypes)] += module.structTypes.len;

        moduleOffsets.items[moduleIdx].counters[@intFromEnum(ModuleStats.ID.pointerTypes)] = total.counters[@intFromEnum(ModuleStats.ID.pointerTypes)];
        total.counters[@intFromEnum(ModuleStats.ID.pointerTypes)] += module.pointerTypes.len;
    }

    var analyzer = Analyzer{
        .functions = ArrayList(Parser.Function.Internal).initCapacity(allocator, total.counters[@intFromEnum(ModuleStats.ID.internalFns)]) catch unreachable,
        .externalFunctions = ArrayList(Parser.Function.External).initCapacity(allocator, total.counters[@intFromEnum(ModuleStats.ID.externalFns)]) catch unreachable,
        .externalLibNames = ArrayList([]const u8).initCapacity(allocator, total.counters[@intFromEnum(ModuleStats.ID.importedModules)]) catch unreachable,
        .externalSymbolNames = ArrayList([]const u8).initCapacity(allocator, total.counters[@intFromEnum(ModuleStats.ID.importedModules)]) catch unreachable,
        .externalLibs = ArrayList(Sem.External.Library).initCapacity(allocator, total.counters[@intFromEnum(ModuleStats.ID.importedModules)]) catch unreachable,
        .importedModules = ArrayList(Parser.ImportedModule).initCapacity(allocator, total.counters[@intFromEnum(ModuleStats.ID.importedModules)]) catch unreachable,
        .integerLiterals = ArrayList(Parser.IntegerLiteral).initCapacity(allocator, total.counters[@intFromEnum(ModuleStats.ID.integerLiterals)]) catch unreachable,
        .arrayLiterals = ArrayList(Parser.ArrayLiteral).initCapacity(allocator, total.counters[@intFromEnum(ModuleStats.ID.arrayLiterals)]) catch unreachable,
        .structLiterals = ArrayList(Parser.StructLiteral).initCapacity(allocator, total.counters[@intFromEnum(ModuleStats.ID.structLiterals)]) catch unreachable,
        .pointerTypes = ArrayList(Type.Pointer).initCapacity(allocator, total.counters[@intFromEnum(ModuleStats.ID.pointerTypes)]) catch unreachable,
        .sliceTypes = ArrayList(Type.Slice).initCapacity(allocator, total.counters[@intFromEnum(ModuleStats.ID.sliceTypes)]) catch unreachable,
        .functionTypes = ArrayList(Type.Function).initCapacity(allocator, total.counters[@intFromEnum(ModuleStats.ID.functionTypes)]) catch unreachable,
        .arrayTypes = ArrayList(Type.Array).initCapacity(allocator, total.counters[@intFromEnum(ModuleStats.ID.arrayTypes)]) catch unreachable,
        .structTypes = ArrayList(Type.Struct).initCapacity(allocator, total.counters[@intFromEnum(ModuleStats.ID.structTypes)]) catch unreachable,
        .unresolvedTypes = ArrayList([]const u8).initCapacity(allocator, total.counters[@intFromEnum(ModuleStats.ID.unresolvedTypes)]) catch unreachable,
        .arrayTypeResolutionBitsets = ArrayList([resolutionBitsetSize]u1).initCapacity(allocator, (total.counters[@intFromEnum(ModuleStats.ID.arrayTypes) / resolutionBitsetSize]) + @intFromBool(total.counters[@intFromEnum(ModuleStats.ID.arrayTypes)] % resolutionBitsetSize != 0)) catch unreachable,
        .structTypeResolutionBitsets = ArrayList([resolutionBitsetSize]u1).initCapacity(allocator, (total.counters[@intFromEnum(ModuleStats.ID.structTypes) / resolutionBitsetSize]) + @intFromBool(total.counters[@intFromEnum(ModuleStats.ID.structTypes)] % resolutionBitsetSize != 0)) catch unreachable,
        .moduleOffset = moduleOffsets.items,
    };

    for (moduleArray) |*module| {
        nextLibName: for (module.libNames) |newLibName| {
            for (analyzer.externalLibNames.items) |existingLibName| {
                if (std.mem.eql(u8, newLibName, existingLibName)) {
                    continue :nextLibName;
                }
            }
            analyzer.externalLibNames.append(newLibName) catch unreachable;
        }
    }
}

test "Analyser.isArrayTypeResolved" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var analyzer = Analyzer{
        .functions = ArrayList(Parser.Function.Internal).init(allocator),
        .externalFunctions = ArrayList(Parser.Function.External).init(allocator),
        .externalLibNames = ArrayList([]const u8).init(allocator),
        .externalSymbolNames = ArrayList([]const u8).init(allocator),
        .externalLibs = ArrayList(Sem.External.Library).init(allocator),
        .importedModules = ArrayList(Parser.ImportedModule).init(allocator),
        .integerLiterals = ArrayList(Parser.IntegerLiteral).init(allocator),
        .arrayTypeResolutionBitsets = ArrayList([resolutionBitsetSize]u1).init(allocator),
        .arrayLiterals = ArrayList(Parser.ArrayLiteral).init(allocator),
        .structLiterals = ArrayList(Parser.StructLiteral).init(allocator),
        .pointerTypes = ArrayList(Type.Pointer).init(allocator),
        .sliceTypes = ArrayList(Type.Slice).init(allocator),
        .functionTypes = ArrayList(Type.Function).init(allocator),
        .arrayTypes = ArrayList(Type.Array).init(allocator),
        .structTypes = ArrayList(Type.Struct).init(allocator),
        .unresolvedTypes = ArrayList([]const u8).init(allocator),
        .structTypeResolutionBitsets = ArrayList([resolutionBitsetSize]u1).init(allocator),
        .moduleOffset = undefined,
    };
    // I swear I don't know what I'm doing
    var bitset = [_][resolutionBitsetSize]u1{[_]u1{0} ** resolutionBitsetSize};
    analyzer.arrayTypeResolutionBitsets.items = &bitset;
    analyzer.arrayTypeResolutionBitsets.items[0][0] = 1;
    try expect(analyzer.isArrayTypeResolved(0) == true);
}

test "Analyzer.isStructTypeResolved" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var analyzer = Analyzer{
        .functions = ArrayList(Parser.Function.Internal).init(allocator),
        .externalFunctions = ArrayList(Parser.Function.External).init(allocator),
        .externalLibNames = ArrayList([]const u8).init(allocator),
        .externalSymbolNames = ArrayList([]const u8).init(allocator),
        .externalLibs = ArrayList(Sem.External.Library).init(allocator),
        .importedModules = ArrayList(Parser.ImportedModule).init(allocator),
        .integerLiterals = ArrayList(Parser.IntegerLiteral).init(allocator),
        .arrayTypeResolutionBitsets = ArrayList([resolutionBitsetSize]u1).init(allocator),
        .arrayLiterals = ArrayList(Parser.ArrayLiteral).init(allocator),
        .structLiterals = ArrayList(Parser.StructLiteral).init(allocator),
        .pointerTypes = ArrayList(Type.Pointer).init(allocator),
        .sliceTypes = ArrayList(Type.Slice).init(allocator),
        .functionTypes = ArrayList(Type.Function).init(allocator),
        .arrayTypes = ArrayList(Type.Array).init(allocator),
        .structTypes = ArrayList(Type.Struct).init(allocator),
        .unresolvedTypes = ArrayList([]const u8).init(allocator),
        .structTypeResolutionBitsets = ArrayList([resolutionBitsetSize]u1).init(allocator),
        .moduleOffset = undefined,
    };

    var bitset = [_][resolutionBitsetSize]u1{[_]u1{0} ** resolutionBitsetSize};
    analyzer.structTypeResolutionBitsets.items = &bitset;
    analyzer.structTypeResolutionBitsets.items[0][0] = 1;
    try expect(analyzer.isStructTypeResolved(0) == true);
}

test "Analyzer.setArrayTypeResolved" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var analyzer = Analyzer{
        .functions = ArrayList(Parser.Function.Internal).init(allocator),
        .externalFunctions = ArrayList(Parser.Function.External).init(allocator),
        .externalLibNames = ArrayList([]const u8).init(allocator),
        .externalSymbolNames = ArrayList([]const u8).init(allocator),
        .externalLibs = ArrayList(Sem.External.Library).init(allocator),
        .importedModules = ArrayList(Parser.ImportedModule).init(allocator),
        .integerLiterals = ArrayList(Parser.IntegerLiteral).init(allocator),
        .arrayTypeResolutionBitsets = ArrayList([resolutionBitsetSize]u1).init(allocator),
        .arrayLiterals = ArrayList(Parser.ArrayLiteral).init(allocator),
        .structLiterals = ArrayList(Parser.StructLiteral).init(allocator),
        .pointerTypes = ArrayList(Type.Pointer).init(allocator),
        .sliceTypes = ArrayList(Type.Slice).init(allocator),
        .functionTypes = ArrayList(Type.Function).init(allocator),
        .arrayTypes = ArrayList(Type.Array).init(allocator),
        .structTypes = ArrayList(Type.Struct).init(allocator),
        .unresolvedTypes = ArrayList([]const u8).init(allocator),
        .structTypeResolutionBitsets = ArrayList([resolutionBitsetSize]u1).init(allocator),
        .moduleOffset = undefined,
    };

    var bitset = [_][resolutionBitsetSize]u1{[_]u1{0} ** resolutionBitsetSize};
    analyzer.arrayTypeResolutionBitsets.items = &bitset;
    analyzer.setArrayTypeResolved(0);
    try expect(analyzer.arrayTypeResolutionBitsets.items[0][0] == 1);
}

test "Analyzer.setStructTypeResolved" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var analyzer = Analyzer{
        .functions = ArrayList(Parser.Function.Internal).init(allocator),
        .externalFunctions = ArrayList(Parser.Function.External).init(allocator),
        .externalLibNames = ArrayList([]const u8).init(allocator),
        .externalSymbolNames = ArrayList([]const u8).init(allocator),
        .externalLibs = ArrayList(Sem.External.Library).init(allocator),
        .importedModules = ArrayList(Parser.ImportedModule).init(allocator),
        .integerLiterals = ArrayList(Parser.IntegerLiteral).init(allocator),
        .arrayTypeResolutionBitsets = ArrayList([resolutionBitsetSize]u1).init(allocator),
        .arrayLiterals = ArrayList(Parser.ArrayLiteral).init(allocator),
        .structLiterals = ArrayList(Parser.StructLiteral).init(allocator),
        .pointerTypes = ArrayList(Type.Pointer).init(allocator),
        .sliceTypes = ArrayList(Type.Slice).init(allocator),
        .functionTypes = ArrayList(Type.Function).init(allocator),
        .arrayTypes = ArrayList(Type.Array).init(allocator),
        .structTypes = ArrayList(Type.Struct).init(allocator),
        .unresolvedTypes = ArrayList([]const u8).init(allocator),
        .structTypeResolutionBitsets = ArrayList([resolutionBitsetSize]u1).init(allocator),
        .moduleOffset = undefined,
    };

    var bitset = [_][resolutionBitsetSize]u1{[_]u1{0} ** resolutionBitsetSize};
    analyzer.structTypeResolutionBitsets.items = &bitset;
    analyzer.setStructTypeResolved(0);
    try expect(analyzer.structTypeResolutionBitsets.items[0][0] == 1);
}
