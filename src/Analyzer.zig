const std = @import("std");
const ArrayList = std.ArrayList;
const Parser = @import("Parser.zig");
const Type = @import("Type.zig");
const Sem = @import("Sem.zig");
const expect = std.testing.expect;
const Allocator = std.mem.Allocator;
const AST = @import("AST.zig").AST;
const Entity = @import("Entity.zig").Entity;
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

    const libCount = analyzer.externalLibNames.items.len;
    var libSymIndices = ArrayList(ArrayList(u32)).initCapacity(allocator, libCount) catch unreachable;
    libSymIndices.appendNTimesAssumeCapacity(ArrayList(u32).init(allocator), libCount);
    analyzer.externalLibs.ensureTotalCapacity(libCount) catch unreachable;

    for (moduleArray) |*module| {
        for (module.libs, 0..) |lib, libIdx| {
            nextSymName: for (lib.symbolNames.items) |newSymName| {
                for (analyzer.externalSymbolNames.items, 0..) |existingSymName, symIdx| {
                    if (std.mem.eql(u8, existingSymName, newSymName)) {
                        var libSymList = &libSymIndices.items[libIdx];
                        for (libSymList.items) |libSymIdx| {
                            if (symIdx == libSymIdx) {
                                continue :nextSymName;
                            }
                        }
                        analyzer.externalSymbolNames.append(newSymName) catch unreachable;
                        libSymList.append(@as(u32, @intCast(symIdx))) catch unreachable;
                    }
                }

                const symbolIdx = @as(u32, @intCast(analyzer.externalSymbolNames.items.len));

                analyzer.externalSymbolNames.append(newSymName) catch unreachable;
                libSymIndices.items[libIdx].append(symbolIdx) catch unreachable;
            }
        }
    }

    for (libSymIndices.items) |librarySymbolList| {
        analyzer.externalSymbolNames.appendNTimesAssumeCapacity(.{ .symbols = librarySymbolList.items });
    }

    for (moduleArray) |*module| {
        for (module.externalFns) |*exFn| {
            const moduleSymName = exFn.declaration.name;
            const moduleLibIdx = exFn.idx.function;
            const moduleLibName = module.libNames[moduleLibIdx];

            exFn.idx = idxBlk: {
                for (analyzer.externalLibNames.items, 0..) |libName, libIdx| {
                    if (std.mem.eql(u8, moduleLibName, libName)) {
                        const library = analyzer.externalLibs.items[libIdx];

                        for (library.symbols) |symIdx| {
                            const symName = analyzer.externalSymbolNames.items[symIdx];
                            if (std.mem.eql(u8, symName, moduleSymName)) {
                                const finalLibIdx = @as(u16, @intCast(libIdx));
                                const finalSymIdx = @as(u16, @intCast(symIdx));
                                break :idxBlk .{
                                    .library = finalLibIdx,
                                    .function = finalSymIdx,
                                };
                            }
                        }
                        unreachable;
                    }
                }
                unreachable;
            };
        }
    }

    for (moduleArray) |*module| {
        analyzer.functions.appendSlice(module.internalFns) catch unreachable;
        analyzer.externalFunctions.appendSlice(module.externalFns) catch unreachable;
        analyzer.importedModules.appendSlice(module.importedModules) catch unreachable;
        analyzer.integerLiterals.appendSlice(module.integerLiterals) catch unreachable;
        analyzer.arrayLiterals.appendSlice(module.arrayLiterals) catch unreachable;
        analyzer.structLiterals.appendSlice(module.structLiterals) catch unreachable;
        analyzer.unresolvedTypes.appendSlice(module.unresolvedTypes) catch unreachable;
        analyzer.arrayTypes.appendSlice(module.arrayTypes) catch unreachable;
        analyzer.structTypes.appendSlice(module.structTypes) catch unreachable;
        analyzer.pointerTypes.appendSlice(module.pointerTypes) catch unreachable;
        analyzer.functionTypes.appendSlice(module.fnTypes) catch unreachable;
        analyzer.sliceTypes.appendSlice(module.sliceTypes) catch unreachable;
    }

    analyzer.arrayTypeResolutionBitsets.items.len = analyzer.arrayTypes.items.len;
    std.mem.set([u64]u1, analyzer.arrayTypeResolutionBitsets.items, std.mem.zeroes([u64]u1));
    analyzer.structTypeResolutionBitsets.items.len = analyzer.structTypes.items.len;
    std.mem.set([u64]u1, analyzer.structTypeResolutionBitsets.items, std.mem.zeroes([u64]u1));

    for (analyzer.functionTypes.items) |*fnType| {
        fnType.returnType = analyzeType(&analyzer, fnType.returnType);
        for (fnType.argTypes) |*argType| {
            argType.* = analyzeType(&analyzer, argType.*);
        }
    }

    for (analyzer.externalFunctions.items) |*func| {
        func.declaration.type.returnType = analyzeType(&analyzer, func.declaration.type.returnType);
        for (func.declaration.type.argTypes) |*argType| {
            argType.* = analyzeType(&analyzer, argType.*);
        }
    }

    for (analyzer.functions.items) |*func| {
        func.declaration.type.returnType = analyzeType(&analyzer, func.declaration.type.returnType);
        for (func.declaration.type.argTypes) |*argType| {
            argType.* = analyzeType(&analyzer, argType.*);
        }
    }

    var moduleIdx: u64 = 0;
    const moduleCount = analyzer.moduleOffset.len;
    while (moduleIdx < moduleCount) : (moduleIdx += 1) {
        const funcRange = getModuleItemSliceRange(.internalFns, &analyzer, moduleIdx);

        for (analyzer.functions.items[funcRange.start..funcRange.end]) |*func| {
            const mainBlock = &func.scopes[0];

            //Todo: implement a way to analyze scopes
            _ = mainBlock;
        }
    }

    return .{
        .functions = analyzer.functions.items,
        .importedModules = analyzer.importedModules.items,
        .integerLiterals = analyzer.integerLiterals.items,
        .arrayLiterals = analyzer.arrayLiterals.items,
        .structLiterals = analyzer.structLiterals.items,
        .external = .{
            .functions = analyzer.externalFunctions.items,
            .libNames = analyzer.externalLibNames.items,
            .symNames = analyzer.externalSymbolNames,
            .libs = analyzer.externalLibs,
        },
        .pointerTypes = analyzer.pointerTypes.items,
        .sliceTypes = analyzer.sliceTypes.items,
        .functionTypes = analyzer.functionTypes.items,
        .arrayTypes = analyzer.arrayTypes.items,
        .structTypes = analyzer.structTypes.items,
    };
}

fn analyzeType(analyzer: *Analyzer, unresolvedType: Type) Type {
    const typeId = unresolvedType.getId();
    const moduleIdx = unresolvedType.getModuleIdx();
    switch (typeId) {
        .Builtin, .Integer => return unresolvedType,
        .Unresolved => {
            const unresolvedTypeModuleOffset = analyzer.moduleOffset[moduleIdx].counters[@intFromEnum(ModuleStats.ID.unresolvedTypes)];
            const idx = unresolvedType.getIdx();
            const unresolvedTypeIdentifier = analyzer.unresolvedTypes.items[unresolvedTypeModuleOffset + idx];

            if (std.mem.eql(u8, unresolvedTypeIdentifier[0], "u")) {
                if (std.fmt.parseUnsigned(u16, unresolvedTypeIdentifier[1..], 10)) |bitCount| {
                    const unsignedIntType = Type.Integer.new(bitCount, .Unsigned);
                    return unsignedIntType;
                }
            } else if (std.mem.eql(u8, unresolvedTypeIdentifier[0], "s")) {
                if (std.fmt.parseUnsigned(u16, unresolvedTypeIdentifier[1..], 10)) |bitCount| {
                    const signedIntType = Type.Integer.new(bitCount, .Signed);
                    return signedIntType;
                }
            }

            for (analyzer.structTypes.items, 0..) |*structType, structIdx| {
                if (std.mem.eql(u8, structType.name, unresolvedTypeIdentifier)) {
                    const resolvedIdx = structIdx;
                    const resolvedStructType = Type.Struct.new(resolvedIdx, moduleIdx);
                    const targetStructType = &analyzer.structTypes.items[resolvedIdx];

                    for (targetStructType.types) |*fieldType| {
                        fieldType.* = analyzeType(analyzer, fieldType.*);
                    }
                    return resolvedStructType;
                }
            }
            unreachable;
        },
        .Pointer => {
            const resolvedIdx = analyzer.moduleOffset[moduleIdx].counters[@intFromEnum(ModuleStats.ID.pointerTypes)] + unresolvedType.getIdx();

            const resolvedPtrType = resolveType(unresolvedType, resolvedIdx);
            var ptrType = &analyzer.pointerTypes.items[resolvedIdx];
            ptrType.type = analyzeType(analyzer, ptrType.type);
            return resolvedPtrType;
        },

        .Array => {
            const resolvedIdx = analyzer.moduleOffset[moduleIdx].counters[@intFromEnum(ModuleStats.ID.arrayTypes)] + unresolvedType.getIdx();

            const resolvedArrayType = resolveType(unresolvedType, resolvedIdx);
            if (!analyzer.isArrayTypeResolved(resolvedIdx)) {
                var arrType = &analyzer.arrayTypes.items[resolvedIdx];
                arrType.type = analyzeType(analyzer, arrType.type);

                var arrayLenExpr = Entity{ .value = arrType.exprLen };
                const arrayLenExprId = arrayLenExpr.getArrayId(.scope);

                const arrayLen = switch (arrayLenExprId) {
                    .integerLiterals => blk: {
                        resolveEntityIdx(analyzer, .integerLiterals, &arrayLenExpr, moduleIdx);
                        break :blk analyzer.integerLiterals.items[arrayLenExpr.getIdx()].value;
                    },
                    else => std.debug.panic("Invalid {}", .{arrayLenExprId}),
                };
                arrType.exprLen = arrayLen;
            }
            return resolvedArrayType;
        },
        .Structure => {
            // this is garbage
            const resolvedIdx = analyzer.moduleOffset[moduleIdx].counters[@intFromEnum(ModuleStats.ID.structTypes)] + unresolvedType.getIdx();

            const resolvedStructType = resolveType(unresolvedType, resolvedIdx);
            if (!analyzer.isStructTypeResolved(resolvedIdx)) {
                analyzer.setStructTypeResolved(resolvedIdx);
            }

            return resolvedStructType;
        },
        else => std.debug.panic("Invalid {}", .{typeId}),
    }
}

pub fn resolveType(T: Type, newIdx: u64) Type {
    var oldT = T;
    oldT.setNewIdx(newIdx);
    oldT.markResolved();
    return oldT;
}

pub fn resolveEntityIdx(analyzer: *Analyzer, comptime moduleStatId: ModuleStats.ID, entity: *Entity, moduleIdx: u64) void {
    const itemRange = getModuleItemSliceRange(moduleStatId, analyzer, moduleIdx);
    const idx = @as(u32, @intCast(itemRange.start)) + entity.getIdx();

    entity.setIdx(idx);
}

pub fn getModuleItemSliceRange(comptime moduleStatId: ModuleStats.ID, analyzer: *Analyzer, moduleIdx: u64) struct { start: u64, end: u64 } {
    const moduleOffsets = analyzer.moduleOffset[moduleIdx];
    const internalItemStart = moduleOffsets.counters[@intFromEnum(moduleStatId)];
    const nextModuleIdx = moduleIdx + 1;
    const internalItemEnd = if (nextModuleIdx < analyzer.moduleOffset.len) analyzer.moduleOffset[nextModuleIdx].counters[@intFromEnum(moduleStatId)] else switch (comptime moduleStatId) {
        .internalFns => analyzer.functions.items.len,
        .externalFns => analyzer.externalFunctions.items.len,
        .integerLiterals => analyzer.integerLiterals.items.len,
        .arrayLiterals => analyzer.arrayLiterals.items.len,
        .importedModules => analyzer.importedModules.items.len,
        else => std.debug.panic("Invalid module Stat Id {}", .{moduleStatId}),
    };
    return .{ .start = internalItemStart, .end = internalItemEnd };
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
