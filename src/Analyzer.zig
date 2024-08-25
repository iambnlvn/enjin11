const std = @import("std");
const ArrayList = std.ArrayList;
const Parser = @import("Parser.zig");
const Type = @import("Type.zig");
const Sem = @import("Sem.zig");
const expect = std.testing.expect;
const Allocator = std.mem.Allocator;
const AST = @import("AST.zig").AST;
const Entity = @import("Entity.zig").Entity;
const EntityId = @import("EntityID.zig").ID;
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

            analyzeScope(&analyzer, mainBlock, func, moduleIdx);
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

pub fn analyzeScope(analyzer: *Analyzer, scope: *Parser.Scope, currentFn: *Parser.Function.Internal, moduleIdx: u64) void {
    const scopeIdx = @as(u32, (@intFromPtr(scope) - @intFromPtr(currentFn.scopes.ptr)) / @sizeOf(Parser.Scope));

    for (scope.statements) |*statement| {
        const statementIdx = statement.getIdx();
        const statementId = statement.getArrayId(.scope);

        switch (statementId) {
            .invokeExpr => {
                const invokeExpr = &scope.InvokeExpr[statementIdx];
                _ = analyzeInvokeExpr(analyzer, currentFn, scope, invokeExpr, moduleIdx);
            },
            .VarDeclaration => {
                var varDec = &scope.VarDeclaration[statementIdx];
                varDec.type = analyzeType(analyzer, varDec.type);
            },
            .Assignment => {
                const assignment = &scope.Assignment[statementIdx];
                analyzeAssignmentExpr(analyzer, currentFn, assignment, moduleIdx);
            },
            .CompoundAssignment => {
                const compoundAssignment = &scope.CompoundAssignment[statementIdx];
                analyzeCompoundAssignment(analyzer, currentFn, compoundAssignment, moduleIdx);
            },
            .Loops => {
                const loop = &scope.Loop[statementIdx];
                const prefScope = &currentFn.scopes[loop.prefixScopeIdx];
                analyzeScope(analyzer, prefScope, currentFn, moduleIdx);
                const bodyScope = &currentFn.scopes[loop.bodyScopeIdx];
                analyzeScope(analyzer, bodyScope, currentFn, moduleIdx);
                const postScope = &currentFn.scopes[loop.postfixScopeIdx];
                analyzeScope(analyzer, postScope, currentFn, moduleIdx);
            },
            .ReturnExpr => {
                const returnExpr = &scope.ReturnExpr[statementIdx];
                const returnType = currentFn.declaration.type.returnType;

                if (returnExpr.expr) |expr2Return| {
                    const retExprArrId = expr2Return.getArrayId(.scope);
                    switch (retExprArrId) {
                        .IntegerLiterals => {
                            analyzeIntegerLiteral(analyzer, &returnExpr.expr.?, returnType, moduleIdx);
                        },
                        .IdentifierExpr => {
                            const identifier = scope.IdentifierExpr[expr2Return.getIdx()];
                            const exprType = resolveIdentifierExpr(analyzer, currentFn, &returnExpr.expr.?, scopeIdx, identifier);

                            if (exprType.value != returnType.value) {
                                std.debug.panic("Type mismatch detected", .{});
                            }
                        },
                        .InvokeExpr => {
                            const invokeExpr = &scope.InvokeExpr[expr2Return.getIdx()];
                            _ = analyzeInvokeExpr(analyzer, currentFn, scope, invokeExpr, moduleIdx);
                        },
                        .ArithmeticExpr => {
                            const arithmeticExpr = &scope.ArithmeticExpr[expr2Return.getIdx()];
                            _ = analyzeArithmeticExpr(analyzer, currentFn, arithmeticExpr, moduleIdx);
                        },
                        .ArraySubExpr => {
                            const arraySubExpr = &scope.ArraySubExpr[expr2Return.getIdx()];
                            _ = analyzeArraySubExpr(analyzer, currentFn, arraySubExpr, moduleIdx, null);
                        },

                        else => std.debug.panic("Invalid", .{}),
                    }
                } else {
                    if (returnType.value != Type.Builtin.voidType.value or returnType.value != Type.Builtin.noreturnType.value) {
                        std.debug.panic("Type mismatch detected, Expected void or noreturn type", .{});
                    }
                }
            },
            .Comparison => {
                const comp = &scope.Comparison[statementIdx];
                _ = analyzeComparison(analyzer, currentFn, comp, moduleIdx);
            },
            .branches => {
                const branch = &scope.Branches[statementIdx];
                const branchCompIdx = branch.condition.getIdx();
                const branchCompArrIdx = branch.condition.getArrayIndex();
                var branchCompScope = &currentFn.scopes[branchCompArrIdx];
                const branchComp = &branchCompScope.Comparison[branchCompIdx];
                _ = analyzeComparison(analyzer, currentFn, branchComp, moduleIdx);
                const ifScope = &currentFn.scopes[branch.ifScope];
                analyzeScope(analyzer, ifScope, currentFn, moduleIdx);

                if (branch.elseScope) |elseScopeIdx| {
                    const elseScope = &currentFn.scopes[elseScopeIdx];
                    analyzeScope(analyzer, elseScope, currentFn, moduleIdx);
                }
            },

            //Todo!: figure out break expressions
            else => std.debug.panic("Invalid", .{}),
        }
    }
}

pub fn analyzeInvokeExpr(analyzer: *Analyzer, currentFn: *Parser.Function.Internal, scope: *Parser.Scope, invokeExpr: *Parser.InvokeExpr, moduleIdx: u64) Type {
    const expr2Invoke = invokeExpr.expr;
    const scopeIdx = expr2Invoke.getArrayIndex();
    const expr2invokeIdx = expr2Invoke.getIdx();
    const expr2InvokeId = expr2invokeIdx.getArrayId(.scope);
    var fnType: Type.Function = undefined;

    const resolvedExpr2Invoke: Entity = blk: {
        if (expr2InvokeId == .IdentifierExpr) {
            const invokeExprName = scope.IdentifierExpr[expr2invokeIdx];
            for (analyzer.functions.items, 0..) |func, funcIdx| {
                if (std.mem.eql(u8, func.declaration.name, invokeExprName)) {
                    continue;
                }
                fnType = func.declaration.type;
                break :blk Entity.new(funcIdx, EntityId.Global.ResolvedInternalFn, 0);
            }
            unreachable;
        } else if (expr2InvokeId == .FieldAccessExpr) {
            const fieldExpr = scope.FieldAccessExpr[expr2invokeIdx];
            const left = fieldExpr.leftExpr;
            const leftIdx = left.getIdx();
            const leftName = scope.IdentifierExpr[leftIdx];

            const field = fieldExpr.fieldExpr;
            const fieldIdx = field.getIdx();
            const fieldName = scope.IdentifierExpr[fieldIdx];
            const importedModuleRange = getModuleItemSliceRange(.importedModules, analyzer, moduleIdx);
            const importedModules = analyzer.importedModules.items[importedModuleRange.start..importedModuleRange.end];

            for (importedModules) |importedModule| {
                if (!std.mem.eql(u8, importedModule.alias.?, leftName)) {
                    continue;
                }
                const importedModuleIdx = importedModule.module.getIdx();

                const internalFnsRange = getModuleItemSliceRange(.internalFns, analyzer, importedModuleIdx);
                const internalFns = analyzer.functions.items[internalFnsRange.start..internalFnsRange.end];

                for (internalFns, 0..) |func, funcIdx| {
                    if (!std.mem.eql(u8, func.declaration.name, fieldName)) {
                        continue;
                    }
                    fnType = func.declaration.type;
                    break :blk Entity.new(funcIdx + internalFnsRange.start, EntityId.Global.ResolvedInternalFn, 0);
                }

                const externalFnsRange = getModuleItemSliceRange(.externalFns, analyzer, importedModuleIdx);
                const externalFns = analyzer.externalFns.items[externalFnsRange.start..externalFnsRange.end];

                for (externalFns, 0..) |func, funcIdx| {
                    if (!std.mem.eql(u8, func.declaration.name, fieldName)) {
                        continue;
                    }
                    fnType = func.declaration.type;
                    break :blk Entity.new(funcIdx + externalFnsRange.start, EntityId.Global.ResolvedExternalFn, 0);
                }
            }
            unreachable;
        } else unreachable;
    };

    invokeExpr.expr = resolvedExpr2Invoke;

    if (invokeExpr.args.len > 0) {
        const argTypes = fnType.argTypes;
        for (invokeExpr.args, 0..) |*arg, argIdx| {
            const arrId = arg.getArrayId(.scope);
            const argType = argTypes[argIdx];

            switch (arrId) {
                .IntegerLiteral => analyzeIntegerLiteral(analyzer, arg, argType, moduleIdx),
                .IdentifierExpr => {
                    const identifier = scope.IdentifierExpr[arg.getIdx()];
                    const exprType = resolveIdentifierExpr(analyzer, currentFn, arg, scopeIdx, identifier);
                    if (argType.value != exprType.value) std.debug.panic("Type mismatch detected", .{});
                },
                else => std.debug.panic("Invalid", .{}),
            }
        }
    }
}

pub fn analyzeIntegerLiteral(analyzer: *Analyzer, entity: *Entity, expectedType: ?Type, moduleIdx: u64) void {
    const type2TypeCheck = expectedType.?;
    if (type2TypeCheck.getId() != .Integer) std.debug.panic("Invalid");
    resolveEntityIdx(analyzer, .integerLiterals, entity, moduleIdx);
}

pub fn resolveIdentifierExpr(analyzer: *Analyzer, currentFn: *Parser.Function.Internal, expr: *Entity, scopeIdx: u32, name: []const u8) Type {
    var currentScopeIdx = scopeIdx;
    for (currentFn.declaration.argNames, 0..) |argName, argIdx| {
        if (std.mem.eql(u8, argName, name)) {
            const argId = Entity.new(argIdx, EntityId.Scope.Args, 0);
            expr.* = argId;
            return currentFn.declaration.type.argTypes[argIdx];
        }
    }
    _ = analyzer;
    var scopeTreeExplored = false;

    while (!scopeTreeExplored) {
        const scope = &currentFn.scopes[currentScopeIdx];
        for (scope.VarDeclaration, 0..) |varDec, varDecIdx| {
            if (std.mem.eql(u8, varDec.name, name)) {
                expr.* = Entity.new(varDecIdx, EntityId.Scope.VarDeclaration, currentScopeIdx);
                return varDec.type;
            }
        }
        scopeTreeExplored = scopeIdx == 0;
        if (!scopeTreeExplored) currentScopeIdx = scope.Parent.scope;
    }
    std.debug.panic("IdentifierExpr {s} not found", .{name});
}

pub fn analyzeBinaryExpr(analyzer: *Analyzer, func: *Parser.Function.Internal, left: *Entity, right: *Entity, moduleIdx: u64) Type {
    const leftType = analyzeTypedExpr(analyzer, func, left, moduleIdx, null);
    const rightType = analyzeTypedExpr(analyzer, func, right, moduleIdx, leftType);

    if (leftType.value != rightType.value) {
        if (leftType.getId() == .Integer and rightType.getId() == .Integer) {
            const leftBits = Type.Integer.getBitCount(leftType);
            const rightBits = Type.Integer.getBitCount(rightType);

            if (leftBits == rightBits) {
                const leftSignedness = Type.Integer.getSignedness(leftType);
                const rightSignedness = Type.Integer.getSignedness(rightType);

                if (leftSignedness == rightSignedness) {
                    unreachable;
                } else {
                    std.debug.panic("Type mismatch due to difference in signedness", .{});
                }
            } else {
                std.debug.panic("Type mismatch due to difference in bit count", .{});
            }
        } else if (leftType.getId() == .Pointer and rightType.getId() == .Pointer) {
            //Todo: fgure out how to compare pointers
            // const leftPtrType = Type.Pointer.getPointeeType(leftType);
            // const rightPtrType = Type.Pointer.getPointeeType(rightType);

            // if (leftPtrType.value != rightPtrType.value) {
            //     std.debug.panic("Type mismatch due to difference in pointee type", .{});
            // }
            return leftType;
        } else {
            std.debug.panic("Type mismatch detected", .{});
        }
    }
    return leftType;
}

pub fn analyzeAssignmentExpr(analyzer: *Analyzer, func: *Parser.Function.Internal, assignment: *Parser.Assignment, moduleIdx: u64) void {
    _ = analyzeBinaryExpr(analyzer, func, &assignment.left, &assignment.right, moduleIdx);
}

pub fn analyzeCompoundAssignment(analyzer: *Analyzer, func: *Parser.Function.Internal, compoundAssignment: *Parser.CompoundAssignment, moduleIdx: u64) void {
    _ = analyzeBinaryExpr(analyzer, func, &compoundAssignment.left, &compoundAssignment.right, moduleIdx);
}

pub fn analyzeArithmeticExpr(analyzer: *Analyzer, func: *Parser.Function.Internal, arithExpr: **Parser.ArithmeticExpr, moduleIdx: u64) void {
    return analyzeBinaryExpr(analyzer, func, &arithExpr.left, &arithExpr.right, moduleIdx);
}

pub fn analyzeComparison(analyzer: *Analyzer, func: *Parser.Function.Internal, comparison: *Parser.Comparison, moduleIdx: u64) Type {
    _ = analyzeBinaryExpr(analyzer, func, &comparison.left, &comparison.right, moduleIdx);
    return Type.Boolean;
}

pub fn analyzeArraySubExpr(analyzer: *Analyzer, func: *Parser.Function.Internal, arraySubExpr: *Parser.ArraySubExpr, moduleIdx: u64, expectedType: ?Type) Type {
    const arrayTypeRef = analyzeTypedExpr(analyzer, func, &arraySubExpr.expr, moduleIdx, null);
    const arrayTypeIdx = arrayTypeRef.getIdx();
    const arrayType = &analyzer.arrayTypes.items[arrayTypeIdx];

    const expectedTypeHint = Type.Integer.new(64, .Unsigned);
    _ = analyzeTypedExpr(analyzer, func, &arraySubExpr.index, moduleIdx, expectedTypeHint);

    const actualType = arrayType.type;

    if (expectedType) |exType| {
        const expected = exType;
        if (expected.getId() != actualType.getId()) {
            std.debug.panic("Type mismatch detected", .{});
        }
    }
    return actualType;
}

pub fn analyzeTypedExpr(analyzer: *Analyzer, func: *Parser.Function.Internal, expr: *Entity, moduleIdx: u64, expectedType: ?Type) Type {
    const exprLevel = expr.getLevel();
    const exprIdx = expr.getIdx();
    const scopeIdx = expr.getArrayIndex();

    const exprType = blk: {
        switch (exprLevel) {
            .scope => {
                const arrayId = expr.getArrayId(.scope);

                switch (arrayId) {
                    .IntegerLiterals => {
                        analyzeIntegerLiteral(analyzer, expr, expectedType, moduleIdx);
                        if (expectedType) |t| {
                            const typeId = t.getId();

                            if (typeId != .integer) {
                                std.debug.panic("expected integer got {}", .{typeId});
                            }
                            break :blk t;
                        } else unreachable;
                    },
                    .ArrayLiterals => {
                        break :blk analyzeArrayLiteral(analyzer, expr, moduleIdx, expectedType);
                    },
                    .VarDeclaration => {
                        var varDec = &func.scopes[scopeIdx].VarDeclaration[exprIdx];
                        varDec.type = analyzeType(analyzer, varDec.type);
                        if (expectedType) |t| {
                            if (varDec.type.value != t.value) {
                                std.debug.panic("Types don't match", .{});
                            }
                        }
                        break :blk varDec.type;
                    },
                    .IdentifierExpr => {
                        const identifier = func.scopes[scopeIdx].IdentifierExpr[exprIdx];
                        const exprType = resolveIdentifierExpr(analyzer, func, expr, scopeIdx, identifier);
                        break :blk exprType;
                    },
                    .InvokeExpr => {
                        const exprScope = &func.scopes[scopeIdx];
                        var invokeExpressions = exprScope.InvokeExpr;
                        const invokeExpr = &invokeExpressions[exprIdx];
                        break :blk analyzeInvokeExpr(analyzer, func, exprScope, invokeExpr, moduleIdx);
                    },
                    .ArithmeticExpr => {
                        var arithExperssionsScope = func.scopes[scopeIdx].ArithmeticExpr;
                        const arithExpr = &arithExperssionsScope[exprIdx];
                        break :blk analyzeArithmeticExpr(analyzer, func, arithExpr, moduleIdx);
                    },
                    .ArraySubscriptExpr => {
                        const arrSubExpr = &func.scopes[scopeIdx].ArraySubExpr[exprIdx];
                        break :blk analyzeArraySubExpr(analyzer, func, arrSubExpr, moduleIdx, expectedType);
                    },
                    .FieldAccessExpr => {
                        const fieldAccessExpr = &func.scopes[scopeIdx].FieldAccessExpr[exprIdx];
                        break :blk analyzeFieldAccessExpr(analyzer, func, &func.scopes[scopeIdx], fieldAccessExpr, moduleIdx);
                    },
                    .structLiterals => {
                        const structTypeRef = expectedType orelse unreachable;
                        const resolvedIdx = analyzer.moduleOffset[moduleIdx].counters[@intFromEnum(ModuleStats.ID.structLiterals)] + exprIdx;

                        var structLiteral = &analyzer.structLiterals.items[resolvedIdx];
                        structLiteral.type = structTypeRef;
                        const structType = &analyzer.structTypes.items[structTypeRef.getIdx()];

                        litField: for (structLiteral.fields.names, 0..) |fieldName, fieldNameIdx| {
                            for (structType.names, 0..) |fieldNameCand, fieldIdx| {
                                if (std.mem.eql(u8, fieldNameCand, fieldName)) {
                                    _ = analyzeTypedExpr(analyzer, func, &structLiteral.fields.initilizers[fieldNameIdx], moduleIdx, structType.types[fieldIdx]);
                                    continue :litField;
                                }
                            }
                            std.debug.panic("No field in struct type", .{});
                        }
                        break :blk structTypeRef;
                    },

                    else => std.debug.panic("Invalid", .{}),
                }
            },
            else => std.debug.panic("unknown level", .{}),
        }
    };
    return exprType;
}

pub fn analyzeArrayLiteral(analyzer: *Analyzer, expr: *Entity, moduleIdx: u64, expectedType: ?Type) Type {
    resolveEntityIdx(analyzer, .arrayLiterals, expr, moduleIdx);
    const arrLiteral = &analyzer.arrayLiterals.items[expr.getIdx()];
    const type2check = expectedType.?;

    const arrType = &analyzer.arrayTypes.items[type2check.getIdx()];

    if (arrType.exprLen != arrLiteral.elements.len) {
        std.debug.panic("Array length mismatch", .{});
    }

    arrLiteral.type = type2check;
    return type2check;
}

fn analyzeFieldAccessExpr(analyzer: *Analyzer, func: *Parser.Function.Internal, arraySubExpr: *Parser.ArraySubExpr, moduleIdx: u64, expectedType: ?Type) Type {
    const arrTypeRef = analyzeTypedExpr(analyzer, func, &arraySubExpr.expr, moduleIdx, null);
    const arrTypeIdx = arrTypeRef.getIdx();
    const arrType = &analyzer.arrayTypes.items[arrTypeIdx];
    const expectedTypeHint = Type.Integer.new(64, .Unsigned);
    _ = analyzeTypedExpr(analyzer, func, &arraySubExpr.idx, moduleIdx, expectedTypeHint);
    const actualType = arrType.type;
    if (expectedType) |type2check| {
        const expected = type2check;
        if (expected.getId() != actualType.getId()) {
            std.debug.panic("Type mismatch detected", .{});
        }
    }
    return actualType;
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
