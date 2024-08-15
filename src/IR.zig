const std = @import("std");
const LLVM = @import("LLVM.zig");
const Type = @import("Type.zig");
const Parser = @import("Parser.zig");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Instruction = @import("Instruction.zig").Instruction;
const Semantics = @import("Sem.zig");
const Entity = @import("Entity.zig").Entity;
const Level = @import("EntityID.zig").ID;

pub const Constant = struct {
    pub const ID = enum(u8) {
        Array,
        @"struct",
        vector,
        AggregateZero,
        DataArray,
        DataVector,
        Fp,
        Int,
        Np,

        const position = Ref.ID.position - @bitSizeOf(ID);
    };

    fn new(id: ID, index: u32) Ref {
        return .{ .value = (@as(u64, @intFromEnum(Ref.ID.constant)) << Ref.ID.position) | (@as(u64, @intFromEnum(id)) << ID.position) | index };
    }

    pub fn getId(reference: Ref) ID {
        return @as(ID, @enumFromInt(@as(u8, @intCast((reference.value & (std.math.maxInt(std.meta.Int(.unsigned, @bitSizeOf(ID))) << ID.position)) >> ID.position))));
    }

    const Struct = struct {
        fields: []Type,
        FieldValues: [][]const u8,
        FieldNames: [][]const u8,
    };
};

pub const Ref = struct {
    const T = u64;
    value: T,

    pub const Null = std.mem.zeroes(T);

    pub const ID = enum(u8) {
        none,
        Module,
        Arg,
        BasicBlock,
        Constant,
        GlobalFunc,
        GlobalVar,
        Instruction,
        Operator,
        ExternalFunc,

        pub const position = @bitSizeOf(T) - @bitSizeOf(ID);
    };
    const LLVMID = LLVM.ID;

    pub fn getId(self: Ref) ID {
        return @as(ID, @enumFromInt(@as(u8, @intCast((self.value & (std.math.maxInt(std.meta.Int(.unsigned, @bitSizeOf(ID))) << ID.position)) >> ID.position))));
    }

    pub fn getIDX(self: Ref) u64 {
        return @as(u64, @truncate(self.value));
    }

    pub fn getSize(self: Ref, program: *const Program) u64 {
        const instructionIdx = self.getIDX();
        const alloc = program.instructions.alloc[instructionIdx];
        const allocType = alloc.baseType;
        return allocType.getSizeResolved(program);
    }
};

pub const Function = struct {
    declaration: Parser.Function,
    basicBlocs: u32,
    argAlloca: []Ref,

    fn newFunction(idx: u32) Ref {
        return .{ .value = (@as(u64, @intFromEnum(Ref.ID.GlobalFunc)) << Ref.ID.position) | idx };
    }

    pub const Builder = struct {
        declaration: Parser.Function,
        basicBlocs: ArrayList(u32),
        argAlloca: ArrayList(Ref),
        returnAlloca: Ref,
        currentBlock: u32,
        refs: refList,
        nextAllocationIdx: u32,
        exitBlock: u32,
        scope2BasicBlockMap: ArrayList(u32),
        isExplicitReturn: bool,
        isEmittedReturn: bool,
        isConditionalAlloca: bool,

        fn new(allocator: *Allocator, function: *Parser.Function.Internal) Function.Builder {
            var builder = Builder{
                .declaration = function.declaration,
                .basicBlocs = ArrayList(u32).init(allocator),
                .argAlloca = ArrayList(Ref).initCapacity(allocator, function.declaration.argNames.len) catch unreachable,
                .returnAlloca = Ref.Null,
                .currentBlock = 0,
                .nextAllocationIdx = 0,
                .exitBlock = 0,
                .scope2BasicBlockMap = ArrayList(u32).initCapacity(allocator, function.scopes.len) catch unreachable,
                .isExplicitReturn = false,
                .isEmittedReturn = false,
                .isConditionalAlloca = false,
            };
            builder.scope2BasicBlockMap.items.len = function.scopes.len;
            return builder;
        }
    };
    pub const Arg = struct {
        pub fn new(idx: u32) Ref {
            return .{ .value = (@as(u64, @intFromEnum(Ref.ID.Arg)) << Ref.ID.position) | idx };
        }
    };

    pub const VarDeclaration = struct {
        fn new(idx: u32) Ref {
            return .{ .value = (@as(u64, @intFromEnum(Ref.ID.GlobalVar)) << Ref.ID.position) | idx };
        }
    };
};

pub const ExternalFunc = struct {
    libName: []const u8,
    type: Type,

    fn new(idx: u32) Ref {
        return .{ .value = (@as(u64, @intFromEnum(Ref.ID.ExternalFunc)) << Ref.ID.position) | idx };
    }
};

pub const BasicBlock = struct {
    instructions: ArrayList(Ref),
    refs: ArrayList(Ref),
    fnIdx: u32,

    fn new(allocator: *Allocator, builder: *Program.Builder) u32 {
        builder.basicBlocks.append(.{
            .instructions = ArrayList(Ref).init(allocator),
            .refs = ArrayList(Ref).init(allocator),
            .fnIdx = 0,
        }) catch unreachable;
        return @as(u32, @intCast(builder.basicBlocks.items.len));
    }

    fn getRef(idx: u32) Ref {
        return .{ .value = (@as(u64, @intFromEnum(Ref.ID.BasicBlock)) << Ref.ID.position) | idx };
    }

    pub fn isTerminated(self: *const BasicBlock) bool {
        if (self.instructions.items.len > 0) {
            const lastInstructionId = Instruction.getId(self.instructions.items[self.instructions.items.len - 1]);

            // break also should be imply a terminated block
            // I think a branch should be considered as a terminated block
            if (lastInstructionId == .ret or lastInstructionId == .br) return true;
        }
        return false;
    }
};

pub const refList = ArrayList(Ref);
pub const Program = struct {
    instructions: struct {
        add: []Instruction.Add,
        sub: []Instruction.Sub,
        mul: []Instruction.Mul,
        load: []Instruction.Load,
        store: []Instruction.Store,
        memCopy: []Instruction.MemCopy,
        call: []Instruction.Call,
        ret: []Instruction.Ret,
        br: []Instruction.Br,
        icmp: []Instruction.Icmp,
        alloc: []Instruction.Alloc,
    },
    functions: []Function,

    functionTypes: []Type.Function,
    arrayTypes: []Type.Array,
    structTypes: []Type.Struct,
    integerLiterals: []Parser.IntegerLiteral,
    instructionReferences: [Instruction.count][]refList,
    arrayLiterals: []Parser.ArrayLiteral,
    structLiterals: []Parser.StructLiteral,
    external: Semantics.External,
    basicBlocks: []BasicBlock,
    pointerTypes: []Type.Pointer,

    pub const Builder = struct {
        const Self = @This();
        instructions: struct {
            add: ArrayList(Instruction.Add),
            sub: ArrayList(Instruction.Sub),
            mul: ArrayList(Instruction.Mul),
            load: ArrayList(Instruction.Load),
            store: ArrayList(Instruction.Store),
            memCopy: ArrayList(Instruction.MemCopy),
            call: ArrayList(Instruction.Call),
            ret: ArrayList(Instruction.Ret),
            br: ArrayList(Instruction.Br),
            icmp: ArrayList(Instruction.Icmp),
            alloc: ArrayList(Instruction.Alloc),
            gep: ArrayList(Instruction.GetElPtr),
        },

        external: Semantics.External,
        functionBuilders: ArrayList(Function.Builder),
        arrayLiterals: ArrayList(Parser.ArrayLiteral),
        structLiterals: ArrayList(Parser.StructLiteral),
        pointerTypes: ArrayList(Type.Pointer),
        integerLiterals: ArrayList(Parser.IntegerLiteral),
        integerLiteralReferences: [Instruction.count][]refList,
        instructionReferences: [Instruction.count]ArrayList(refList),
        functionTypes: ArrayList(Type.Function),
        arrayTypes: ArrayList(Type.Array),
        structTypes: ArrayList(Type.Array),
        sliceTypes: ArrayList(Type.Slice),
        basicBlocks: ArrayList(BasicBlock),
        currentFunction: u32,

        fn new(allocator: *Allocator, result: Semantics.Result) Self {
            var builder = Builder{
                .instructions = .{
                    .add = ArrayList(Instruction.Add).init(allocator),
                    .sub = ArrayList(Instruction.Sub).init(allocator),
                    .mul = ArrayList(Instruction.Mul).init(allocator),
                    .load = ArrayList(Instruction.Load).init(allocator),
                    .store = ArrayList(Instruction.Store).init(allocator),
                    .memCopy = ArrayList(Instruction.MemCopy).init(allocator),
                    .call = ArrayList(Instruction.Call).init(allocator),
                    .ret = ArrayList(Instruction.Ret).init(allocator),
                    .icmp = ArrayList(Instruction.Icmp).init(allocator),
                    .alloc = ArrayList(Instruction.Alloc).init(allocator),
                    .gep = ArrayList(Instruction.GetElPtr).init(allocator),
                },

                .instructionRefrences = blk: {
                    var insRef: [Instruction.count]ArrayList(refList) = undefined;
                    std.mem.set(ArrayList(refList), insRef[0..], ArrayList(refList).init(allocator));
                    break :blk insRef;
                },

                .external = result.external,
                .basicBlocks = ArrayList(BasicBlock).init(allocator),
                .pointerTypes = ArrayList(Type.Pointer).init(allocator),
                .functionBuilders = ArrayList(Function.Builder).initCapacity(allocator, result.functions.len) catch unreachable,
                .arrayLiterals = ArrayList(Parser.ArrayLiteral).init(allocator),
                .structLiterals = ArrayList(Parser.StructLiteral).init(allocator),
                .functionTypes = ArrayList(Type.Function).init(allocator),
                .arrayTypes = ArrayList(Type.Array).init(allocator),
                .structTypes = ArrayList(Type.Struct).init(allocator),
                .sliceTypes = ArrayList(Type.Slice).init(allocator),
                .integerLiterals = ArrayList(Parser.IntegerLiteral).init(allocator),
                .integerLiteralReferences = ArrayList(refList).initCapacity(allocator, result.integerLiterals.len) catch unreachable,
                .currentFunction = 0,
            };

            builder.functionBuilders.items.len = result.functions.len;

            for (result.functions, 0..) |*fnAst, fnIdx| {
                const fnBuilder = &builder.functionBuilders.items[fnIdx];
                fnBuilder.* = Function.Builder.new(allocator, fnAst);
            }

            builder.integerLiterals.appendSlice(result.integerLiterals) catch unreachable;
            builder.integerLiteralReferences.items.len = result.integerLiterals.len;
            builder.arrayLiterals.appendSlice(result.arrayLiterals) catch unreachable;
            builder.structLiterals.appendSlice(result.structLiterals) catch unreachable;

            for (builder.integerLiteralReferences.items) |*ref| {
                ref.* = refList.init(allocator);
            }

            builder.arrayTypes.appendSlice(result.arrayTypes) catch unreachable;
            builder.structTypes.appendSlice(result.structTypes) catch unreachable;
            builder.sliceTypes.appendSlice(result.sliceTypes) catch unreachable;
            builder.functionTypes.appendSlice(result.functionTypes) catch unreachable;

            return builder;
        }

        pub fn appendRef(self: *Self, value: Ref, refrence: Ref) void {
            switch (value.getId()) {
                .Constant => {
                    switch (Constant.getId(value)) {
                        .Int => {
                            self.integerLiteralReferences[value.getIDX()].append(refrence) catch unreachable;
                        },
                        .Array => {
                            self.arrayLiterals[value.getIDX()].refs.append(refrence) catch unreachable;
                        },
                        .@"struct" => {
                            self.structLiterals[value.getIDX()].refs.append(refrence) catch unreachable;
                        },
                        else => std.debug.panic("Unsupported constant type"),
                    }
                },
                .Instruction => {
                    self.instructionReferences[@intFromEnum(Instruction.getId(value))].items[value.getIDX()].append(refrence) catch unreachable;
                },
                .GlobalFunc => {
                    self.functionBuilders.items[value.getIDX()].refs.append(refrence) catch unreachable;
                },
                // todo: add more cases
                else => std.debug.panic("Unsupported ref type"),
            }
        }

        pub fn appendInstruction2fn(self: *Program.Builder, instruction: Ref) Ref {
            const blockIdx = self.functionBuilders.items[self.currentFunction].currentBlock;
            const currentBlock = &self.basicBlocks.items[blockIdx];
            currentBlock.instructions.append(instruction) catch unreachable;
            return instruction;
        }

        pub fn getCurrentBasicBlock(self: *Builder) *BasicBlock {
            return &self.basicBlocks.items[self.functionBuilders.items[self.currentFunction].currentBlock];
        }

        pub fn getOrCreatePtrType(self: *Self, ptrType: Type) Type {
            const ptrT = Type.Pointer.new(self.pointerTypes.items.len, 0);
            self.pointerTypes.append(.{
                .type = ptrType,
            }) catch unreachable;
            return ptrT;
        }
        pub fn processExpr(self: *Self, allocator: *Allocator, fnBuilder: *Function.Builder, res: Semantics.Result, ast: Entity) Ref {
            const astLevel = ast.getLevel();
            switch (astLevel) {
                .global => {
                    const globalExprId = ast.getArrayId(Level.Global);
                    switch (globalExprId) {
                        .ResolvedInternalFn => {
                            unreachable;
                        },
                        else => std.debug.panic("Unsupported global expression"),
                    }
                },
                .scope => {
                    const scopeIdx = ast.getArrayIndex();
                    const exprIdx = ast.getIdx();
                    const exprId = ast.getArrayId(Level.Scope);
                    switch (exprId) {
                        .IntegerLiterals => {
                            return Constant.new(.Int, exprIdx);
                        },

                        .Args => {
                            const argIdx = exprIdx;
                            const allocRef = fnBuilder.argAlloca.items[exprIdx];
                            const argType = fnBuilder.declaration.type.argTypes[argIdx];
                            return Instruction.Load.new(allocator, self, argType, allocRef);
                        },

                        .ArrayLiterals => {
                            return Constant.new(.Array, exprIdx);
                        },
                        .ArraySubscriptExpr => {
                            const arraySubscriptExpr = &res.functions[self.currentFunction].scopes[scopeIdx].ArraySubscriptExpr[exprIdx];
                            var arrayElType: Type = undefined;
                            const gep = self.retreiveGetElementPtrFromArraySub(allocator, fnBuilder, arraySubscriptExpr, &arrayElType);

                            return Instruction.Load.new(allocator, self, arrayElType, gep);
                        },
                        .FieldAccessExpr => {
                            const fieldAccessExpr = &res.functions[self.currentFunction].scopes[scopeIdx].FieldAccessExpr[exprIdx];
                            var structElType: Type = undefined;
                            const getElPtr = self.retrieveGetElementPtrfromFieldAccess(allocator, fnBuilder, fieldAccessExpr, &structElType);

                            return Instruction.Load.new(allocator, self, structElType, getElPtr);
                        },
                        .StructLiterals => {
                            return Constant.new(.@"struct", exprIdx);
                        },

                        .VarDeclaration => {
                            const allocRef = self.findExprAlloc(fnBuilder, ast);
                            const alloc = self.instructions.alloc.items[allocRef.getIDX()];

                            return Instruction.Load.new(allocator, self, alloc.baseType, allocRef);
                        },

                        .ArithmeticExpr => {
                            const arithmeticExpr = &res.functions[self.currentFunction].scopes[scopeIdx].ArithmeticExpr[exprIdx];

                            const lhs = self.processExpr(allocator, fnBuilder, res, arithmeticExpr.left);
                            const rhs = self.processExpr(allocator, fnBuilder, res, arithmeticExpr.right);

                            return switch (arithmeticExpr.id) {
                                .Add => Instruction.Add.new(allocator, self, lhs, rhs),
                                .Sub => Instruction.Sub.new(allocator, self, lhs, rhs),
                                .Mul => Instruction.Mul.new(allocator, self, lhs, rhs),
                                //Todo: Implement the rest of the arithmetic expressions
                                // .Div => Instruction.Div.new(allocator, self, lhs, rhs),
                                // .Mod => Instruction.Mod.new(allocator, self, lhs, rhs),
                                else => std.debug.panic("Unsupported arithmetic expression"),
                            };
                        },
                        .InvokeExpr => return self.processInvokeExpr(allocator, fnBuilder, res, ast),
                        //todo: add more cases
                        else => std.debug.panic("Unsupported scope expression"),
                    }
                },
                else => std.debug.panic("Unsupported expression level"),
            }
        }

        pub fn processInvokeExpr(self: *Builder, allocator: *Allocator, fnBuilder: *Function.Builder, res: Semantics.Result, ast: Entity) Ref {
            const scopeIdx = ast.getArrayIndex();
            const invokeExprIdx = ast.getIdx();
            const astInvokeExpr = &res.functions[self.currentFunction].scopes[scopeIdx].InvokeExpr[invokeExprIdx];
            const expr = astInvokeExpr.expr;
            var calledFnRef: Ref = undefined;
            var astCalledFnDeclaration: Parser.Function = undefined;
            const calledFnIdx = expr.getIdx();
            switch (expr.getArrayIndex(.GlobalFunc)) {
                .ResolvedInternalFn => {
                    calledFnRef = Function.newFunction(calledFnIdx);
                    astCalledFnDeclaration = res.functions[calledFnIdx].declaration;
                },
                .ResolvedExternalFn => {
                    calledFnRef = ExternalFunc.new(calledFnIdx);
                    astCalledFnDeclaration = res.external.functions[calledFnIdx];
                },
                else => unreachable,
            }
            const calledFnType = astCalledFnDeclaration.type;
            const calledFnArgCount = astCalledFnDeclaration.argNames.len;
            if (calledFnArgCount > 0) {
                var argList = ArrayList(Ref).initCapacity(allocator, calledFnArgCount) catch unreachable;

                for (astInvokeExpr.args) |arg| {
                    const argLevel = arg.getLevel();
                    const astIdx = arg.getIdx();

                    switch (argLevel) {
                        .scope => {
                            const argId = arg.getArrayId(.scope);
                            switch (argId) {
                                .IntegerLiterals => {
                                    argList.append(Constant.new(.Int, astIdx));
                                },
                                .VarDeclaration => {
                                    const argAllocaRef = self.findExprAlloc(fnBuilder, arg) orelse unreachable;
                                    const argAlloc = self.instructions.alloc.items[argAllocaRef.getIDX()];
                                    const argLoad = Instruction.Load.new(allocator, self, argAlloc.baseType, argAllocaRef);
                                    argList.append(argLoad) catch unreachable;
                                },
                                else => std.debug.panic("Unsupported", .{argId}),
                                // something like an address expr can be added here in case we are passing addr
                            }
                        },
                        else => std.debug.panic("Incorrect levelf", .{argLevel}),
                    }
                }
                return Instruction.new(allocator, self, calledFnType.returnType, calledFnRef, argList.items);
            } else {
                return Instruction.new(allocator, self, calledFnType.returnType, calledFnRef, std.mem.zeroes([]Ref));
            }
        }

        pub fn findExprAlloc(self: *Program.Builder, fnBuilder: *Function.Builder, expr: Entity) ?Ref {
            const entryBlockIdx = fnBuilder.basicBlocs.items[0];
            const entryBlock = self.basicBlocks.items[entryBlockIdx];
            const allocInstruction = entryBlock.instructions.items[@intFromBool(fnBuilder.isConditionalAlloca)..fnBuilder.nextAllocationIdx];
            const exprIdx = expr.getIdx();

            for (allocInstruction) |fnAllocRef| {
                const allocation = self.instructions.alloc.items[fnAllocRef.getIDX()];
                const allocaRefIdx = allocation.ref.getIDX();

                if (allocaRefIdx == exprIdx) {
                    return fnAllocRef;
                }
            }
            return null;
        }

        pub fn retreiveGetElementPtrFromArraySub(self: *Builder, allocator: *Allocator, fnBuilder: *Function.Builder, arraySubscriptExpr: *Parser.ArraySubExpr, arrayElType: Type) Ref {
            const idxExpr = self.getConstantInt();

            const arrayExprAlloc = self.findExprAlloc(fnBuilder, arraySubscriptExpr.expr) orelse unreachable;
            const alloc = self.instructions.alloc.items[arrayExprAlloc.getIDX()];
            const arrayTypeRef = alloc.baseType;

            const arrayType = self.arrayTypes.items[arrayTypeRef.getIDX()];
            arrayElType.* = arrayType.elementType;

            var indices = ArrayList(i64).initCapacity(allocator, 2) catch unreachable;
            indices.appendAssumeCapacity(0);
            indices.appendAssumeCapacity(idxExpr);

            return Instruction.GetElPtr.new(allocator, self, arrayElType.*, arrayExprAlloc, indices.items);
        }

        pub fn retrieveGetElementPtrfromFieldAccess(self: *Self, allocator: *Allocator, fnBuilder: *Function.Builder, fieldAccessExpr: *Parser.FieldAccessExpr, structElType: Type) Ref {
            var structExprAlloc = Self.findExprAlloc(fnBuilder, fieldAccessExpr.leftExpr) orelse unreachable;
            const alloc = self.instructions.alloc.items[structExprAlloc.getIDX()];
            var fieldTypeRef = alloc.baseType;

            if (fieldTypeRef.getId() == .Pointer) {
                structExprAlloc = Instruction.Load.new(allocator, self, fieldTypeRef, structExprAlloc);
                fieldTypeRef = Type.Pointer.getBaseType(fieldTypeRef, self.pointerTypes.items);
            }
            const structType = self.structTypes.items[fieldTypeRef.getIDX()];
            const fieldIdx = fieldAccessExpr.fieldExpr.getIdx();
            const fieldType = structType.types[fieldIdx];
            structElType.* = fieldType;

            var indices = ArrayList(i64).initCapacity(allocator, 2) catch unreachable;
            indices.appendAssumeCapacity(0);
            indices.appendAssumeCapacity(fieldIdx);

            return Instruction.GetElPtr.new(allocator, self, structElType.*, structExprAlloc, indices.items);
        }

        pub fn getConstantInt(self: *Builder, expr: Entity) i64 {
            const level = expr.getLevel();
            switch (level) {
                .scope => {
                    const arrayId = expr.getArrayId(.scope);

                    switch (arrayId) {
                        .IntegerLiterals => {
                            const intLiteral = self.integerLiterals.items[expr.getIdx()];

                            if (intLiteral.isSigned) {
                                return -@as(i64, @intCast(intLiteral.value));
                            } else {
                                return @as(i64, @intCast(intLiteral.value));
                            }
                        },

                        else => std.debug.panic("Invalid", .{}),
                    }
                },

                else => std.debug.panic("Invalid level {any}", .{level}),
            }
        }

        pub fn processScope(self: *Builder, allocator: *Allocator, fnBuilder: *Function.Builder, scopeIdx: u32, res: Semantics.Result, existingBlock: ?u32) void {
            fnBuilder.currentBlock = existingBlock orelse blk: {
                const blockIdx = BasicBlock.new(allocator, self);
                self.appendBlock2CurrentFn(blockIdx, scopeIdx);

                break :blk blockIdx;
            };

            const scope = res.functions[self.currentFunction].scopes[scopeIdx];
            const returnType = fnBuilder.declaration.type.returnType;

            for (scope.statements) |statementAst| {
                if (!fnBuilder.isEmittedReturn) {
                    const statementIdx = statementAst.getIdx();
                    const statementLevel = statementAst.getLevel();

                    switch (statementLevel) {
                        .scope => {
                            const statementType = statementAst.getArrayId(.scope);

                            switch (statementType) {
                                .InvokeExpr => {
                                    self.processInvokeExpr(allocator, fnBuilder, res, statementAst);
                                },
                                .VarDeclaration => {
                                    const varDec = scope.VarDeclaration[statementIdx];
                                    const varType = varDec.type;

                                    Instruction.Alloc.new(allocator, self, Function.VarDeclaration.new(statementIdx), varType, null);
                                },
                                .ReturnExpr => {
                                    fnBuilder.isEmittedReturn = true;
                                    const retAstExpr = scope.ReturnExpr[statementIdx];

                                    if (retAstExpr.expr) |ast2Return| {
                                        const retExpr = self.processExpr(allocator, fnBuilder, res, ast2Return);

                                        if (fnBuilder.isConditionalAlloca) {
                                            _ = Instruction.Store.new(allocator, self, fnBuilder.returnAlloca, returnType);

                                            Instruction.Br.new(allocator, self, fnBuilder.exitBlock);
                                        } else {
                                            _ = Instruction.Ret.new(allocator, self, returnType, retExpr);
                                        }
                                    } else {
                                        if (!fnBuilder.isExplicitReturn) {
                                            _ = Instruction.Ret.new(allocator, self, returnType, null);

                                            fnBuilder.isEmittedReturn = true;
                                        } else {
                                            unreachable;
                                        }
                                    }
                                },
                                .Assignment => {
                                    const assignment = scope.Assignment[statementIdx];
                                    var storeType: Type = undefined;

                                    const leftId = assignment.left.getArrayId(.scope);

                                    switch (leftId) {
                                        .VarDeclaration => {
                                            const allocRef = self.findExprAlloc(fnBuilder, assignment.left) orelse unreachable;
                                            const alloc = self.instructions.alloc.items[allocRef.getIDX()];
                                            storeType = alloc.baseType;

                                            switch (storeType.getId()) {
                                                .Array, .Structure => {
                                                    const rightRef = self.processExpr(allocator, fnBuilder, res, assignment.right);
                                                    const memCopySize = storeType.getSize(self);
                                                    _ = Instruction.MemCopy.new(allocator, self, allocRef, rightRef, memCopySize);
                                                },
                                                else => {
                                                    const rightRef = self.processExpr(allocator, fnBuilder, res, assignment.right);
                                                    _ = Instruction.Store.new(allocator, self, allocRef, rightRef, storeType);
                                                },
                                            }
                                        },
                                        .ArraySubscriptExpr => {
                                            const arraySubscriptExpr = &res.functions[self.currentFunction].scopes[scopeIdx].ArraySubExpr[assignment.left.getIdx()];
                                            var arrElType: Type = undefined;

                                            const gep = self.retreiveGetElementPtrFromArraySub(allocator, fnBuilder, arraySubscriptExpr, &arrElType);
                                            const rightRef = self.processExpr(allocator, fnBuilder, res, assignment.right);

                                            _ = Instruction.new(allocator, self, rightRef, gep, arrElType);
                                        },
                                        else => std.debug.panic("Unsupported assignment type"),
                                    }
                                },
                                .CompoundAssignment => {
                                    const compoundAssignment = scope.CompoundAssignment[statementIdx];
                                    const left = self.processExpr(allocator, fnBuilder, res, compoundAssignment.left);
                                    const right = self.processExpr(allocator, fnBuilder, res, compoundAssignment.right);

                                    const op = switch (compoundAssignment.id) {
                                        .add => Instruction.Add.new(allocator, self, left, right),
                                        .sub => Instruction.Sub.new(allocator, self, left, right),
                                        .mul => Instruction.Mul.new(allocator, self, left, right),
                                        else => std.debug.panic("Unsupported compound assignment"),
                                    };

                                    const leftId = compoundAssignment.left.getArrayId(.scope);
                                    const leftAlloc = switch (leftId) {
                                        .VarDeclaration => self.findExprAlloc(fnBuilder, compoundAssignment.left) orelse unreachable,

                                        else => std.debug.panic("Unsupported", .{}),
                                    };
                                    const alloc = self.instructions.alloc.items[leftAlloc.getIDX()];
                                    _ = Instruction.Store.new(allocator, self, op, leftAlloc, alloc.baseType);
                                },
                                .Loops => {
                                    const ast = scope.Loop[statementIdx];
                                    const prefixBlock = BasicBlock.new(allocator, self);
                                    const body = BasicBlock.new(allocator, self);
                                    const postfixBlock = BasicBlock.new(allocator, self);
                                    const endBlock = BasicBlock.new(allocator, self);

                                    const fnAst = &res.functions[self.currentFunction];
                                    const prefixScopeAst = fnAst.scopes[ast.prefixScopeIdx];

                                    if (prefixScopeAst.statements.len != 1) {
                                        std.debug.panic("Invalid loop prefix scope");
                                    }

                                    self.appendBlock2CurrentFn(prefixBlock, ast.prefixScopeIdx);
                                    Instruction.Br.new(allocator, self, prefixBlock);

                                    const conditionAst = prefixScopeAst.statements[0];
                                    const condition = self.processComparison(allocator, fnBuilder, res, conditionAst);
                                    Instruction.Br.newConditional(allocator, self, condition, body, endBlock);

                                    self.appendBlock2CurrentFn(body, ast.bodyScopeIdx);
                                    self.processScope(allocator, fnBuilder, ast.bodyScopeIdx, res, body);

                                    Instruction.Br.new(allocator, self, postfixBlock);

                                    self.appendBlock2CurrentFn(postfixBlock, ast.prefixScopeIdx);

                                    self.processScope(allocator, fnBuilder, ast.postfixScopeId, res, postfixBlock);

                                    if (!fnBuilder.isEmittedReturn) {
                                        Instruction.Br.new(allocator, self, prefixBlock);
                                        self.appendBlock2CurrentFn(endBlock, null);
                                        fnBuilder.currentBlock = endBlock;
                                    }
                                },
                                .Branches => {
                                    const ast = scope.Branches[statementIdx];
                                    const branchCondition = self.processComparison(allocator, fnBuilder, res, ast.condition);

                                    const ifBlock = BasicBlock.new(allocator, self);
                                    const exitBlock = BasicBlock.new(allocator, self);
                                    const elseBlock = if (ast.elseScope) |_| BasicBlock.new(allocator, self) else exitBlock;

                                    var isExitBlockUsed = true;

                                    Instruction.Br.newConditional(allocator, self, branchCondition, ifBlock, elseBlock);
                                    self.appendBlock2CurrentFn(ifBlock, ast.ifScope);

                                    fnBuilder.isEmittedReturn = false;

                                    self.processScope(allocator, fnBuilder, ast.ifScope, res, ifBlock);

                                    const isIfBlockReturn = fnBuilder.isEmittedReturn;
                                    Instruction.Br.new(allocator, self, exitBlock);

                                    fnBuilder.isEmittedReturn = false;

                                    if (exitBlock != exitBlock) {
                                        self.appendBlock2CurrentFn(elseBlock, ast.elseScope);
                                        self.processScope(allocator, fnBuilder, ast.elseScope.?, res, elseBlock);

                                        Instruction.Br.new(allocator, self, exitBlock);
                                    }

                                    const isElseBlockReturn = fnBuilder.isEmittedReturn;
                                    // It doesn't make sense to have a return in both the if and else block
                                    fnBuilder.isEmittedReturn = isIfBlockReturn and isElseBlockReturn;

                                    if (isExitBlockUsed and !fnBuilder.isEmittedReturn) {
                                        self.appendBlock2CurrentFn(exitBlock, null);
                                        fnBuilder.currentBlock = exitBlock;
                                    }
                                },
                                .BreakExpr => {
                                    const ast = scope.BreakExpr[statementIdx];

                                    const loopRef = ast.loop2Break;
                                    const loopIdx = loopRef.getIdx();
                                    const loopScope = loopRef.getArrayIndex();

                                    const loopAst = &res.functions[self.currentFunction].scopes[loopScope].Loop[loopIdx];

                                    const loopPrefixIdx = fnBuilder.scope2BasicBlockMap.items[loopAst.prefixScopeIdx];
                                    const loopPrefixBlock = self.basicBlocks.items[loopPrefixIdx];

                                    const BreakInstruction = loopPrefixBlock.instructions.items[loopPrefixBlock.instructions.items.len - 1];
                                    const br = self.instructions.br.items[BreakInstruction.getIDX()];

                                    Instruction.Br.new(allocator, self, br.falsyDestBlock.?);
                                },
                                else => std.debug.panic("Unsupported statement type"),
                            }
                        },
                        else => std.debug.panic("Unsupported statement level"),
                    }
                }
            }
        }

        pub fn appendBlock2CurrentFn(self: *Program.Builder, basicBlockIdx: u32, scopeIdx: ?u32) void {
            var fnBuilder = &self.functionBuilders.items[self.currentFunction];
            var block = &self.basicBlocks.items[basicBlockIdx];
            const fnBlockIdx = @as(u32, @intCast(fnBuilder.basicBlocs.items.len));
            block.fnIdx = fnBlockIdx;
            fnBuilder.basicBlocs.append(basicBlockIdx) catch unreachable;
            if (scopeIdx) |scope| {
                fnBuilder.scope2BasicBlockMap.items[scope] = basicBlockIdx;
            }
        }

        pub fn processComparison(self: *Builder, allocator: *Allocator, fnBuilder: *Function.Builder, res: Semantics.Result, comparisonRef: Entity) Ref {
            const scopeIdx = comparisonRef.getArrayIndex();
            const compIdx = comparisonRef.getIdx();
            const comp = &res.functions[self.currentFunction].scopes[scopeIdx].Comparison[compIdx];

            const left = self.processExpr(allocator, fnBuilder, res, comp.left);
            const right = self.processExpr(allocator, fnBuilder, res, comp.right);

            const compId: Instruction.Icmp.Id = switch (comp.id) {
                .LessThan => .slt,
                .GreaterThan => .sgt,
                .LessThanOrEqual => .sle,
                .GreaterThanOrEqual => .sge,
                .Equal => .eq,
                .NotEqual => .ne,
                else => std.debug.panic("Unsupported comparison"),
            };
            return Instruction.Icmp.new(allocator, self, compId, left, right);
        }
    };
};
