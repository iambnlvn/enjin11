const Ref = @import("IR.zig").Ref;
const std = @import("std");
const Allocator = std.mem.Allocator;
const Ir = @import("IR.zig");
const ArrayList = std.ArrayList;
const Type = @import("Type.zig").Type;
pub const Instruction = struct {
    pub const count = @intFromEnum(ID.memCopy) + 1;

    pub fn getId(reference: Ref) ID {
        return @as(ID, @enumFromInt(@as(u8, @intCast((reference.value & (std.math.maxInt(std.meta.Int(.unsigned, @bitSizeOf(ID))) << ID.position)) >> ID.position))));
    }

    pub fn new(allocator: *Allocator, builder: *Ir.Program.Builder, id: ID, index: u64) Ref {
        builder.instructionReferences[@enumFromInt(id)].append(ArrayList(Ref).init(allocator)) catch unreachable;

        return .{ .value = (@as(u64, @intFromEnum(Ref.ID.Instruction)) << Ref.ID.position) | (@as(u64, @intFromEnum(id)) << Instruction.ID.position) | index };
    }

    pub const ID = enum(u8) {
        add = 0,
        sub,
        fsub,
        mul,
        fmul,
        udiv,
        sdiv,
        urem,
        srem,
        frem,
        memCopy,
        call,
        ret,
        br,
        icmp,
        alloc,
        getElPtr,
        load,
        //TODO: implement more instructions
        const position = Ref.ID.position - @bitSizeOf(Instruction.ID);
    };
    pub const Add = struct {
        left: Ref,
        right: Ref,

        fn new(allocator: *Allocator, builder: *Ir.Program.Builder, left: Ref, right: Ref) Ref {
            var list = &builder.instructions.add;
            const arrayIdx = list.items.len;
            list.append(Instruction.Add{ .left = left, .right = right }) catch unreachable;
            const instruction = Instruction.new(allocator, builder, .add, arrayIdx);
            builder.appendRef(left, instruction);
            builder.appendRef(right, instruction);
            return builder.appendInstruction2fn(instruction);
        }
    };

    pub const Sub = struct {
        left: Ref,
        right: Ref,

        fn new(allocator: *Allocator, builder: *Ir.Program.Builder, left: Ref, right: Ref) Ref {
            var list = &builder.instructions.sub;
            const arrayIdx = list.items.len;
            list.append(Instruction.Add{ .left = left, .right = right }) catch unreachable;
            const instruction = Instruction.new(allocator, builder, .sub, arrayIdx);
            builder.appendRef(left, instruction);
            builder.appendRef(right, instruction);
            return builder.appendInstruction2fn(instruction);
        }
    };

    pub const Mul = struct {
        left: Ref,
        right: Ref,

        fn new(allocator: *Allocator, builder: *Ir.Program.Builder, left: Ref, right: Ref) Ref {
            var list = &builder.instructions.mul;
            const arrayIdx = list.items.len;
            list.append(Instruction.Add{ .left = left, .right = right }) catch unreachable;
            const instruction = Instruction.new(allocator, builder, .mul, arrayIdx);
            builder.appendRef(left, instruction);
            builder.appendRef(right, instruction);
            return builder.appendInstruction2fn(instruction);
        }
    };

    pub const Load = struct {
        type: Type,
        pointer: Ref,

        fn new(allocator: *Allocator, builder: *Ir.Program.Builder, loadType: Type, loadValue: Ref) Ref {
            const loadInstruction = Instruction.new(allocator, builder, .load, builder.instructions.load.items.len);
            builder.instructions.load.append(Instruction.Load{
                .type = loadType,
                .pointer = loadValue,
            }) catch unreachable;
            builder.appendRef(loadValue, loadInstruction);
            return builder.appendInstruction2fn(loadInstruction);
        }
    };

    pub const Store = struct {
        value: Ref,
        pointer: Ref,
        type: Type,

        fn new(allocator: *Allocator, builder: *Ir.Program.Builder, value: Ref, pointer: Ref, storeType: Type) Ref {
            builder.instructions.store.append(.{
                .value = value,
                .pointer = pointer,
                .type = storeType,
            }) catch unreachable;

            const storeInstruction = Instruction.new(allocator, builder, .store, builder.instructions.store.items.len);
            builder.appendRef(pointer, storeInstruction);
            builder.appendRef(value, storeInstruction);
            return builder.appendInstruction2fn(storeInstruction);
        }
    };

    pub const MemCopy = struct {
        source: Ref,
        destination: Ref,
        size: u64,

        fn new(allocator: *Allocator, builder: *Ir.Program.Builder, src: Ref, dest: Ref, size: u64) Ref {
            var list = &builder.instructions.memCopy;
            list.append(.{ .source = src, .destination = dest, .size = size }) catch unreachable;

            const instruction = Instruction.new(allocator, builder, .memCopy, list.items.len);
            builder.appendRef(dest, instruction);
            builder.appendRef(src, instruction);

            builder.appendInstruction2fn(instruction);
        }
    };

    pub const Call = struct {
        type: Type,
        callee: Ref,
        args: []Ref,

        fn new(allocator: *Allocator, builder: *Ir.Program.Builder, returnType: Type, callee: Ref, args: []Ref) Ref {
            builder.instructions.call.append(.{
                .type = returnType,
                .callee = callee,
                .args = args,
            }) catch unreachable;

            const instruction = Instruction.new(allocator, builder, .call, builder.instructions.call.items.len);
            builder.appendRef(callee, instruction);

            for (args) |arg| {
                builder.appendRef(arg, instruction);
            }

            return builder.appendInstruction2fn(instruction);
        }
    };

    pub const Ret = struct {
        type: Type,
        value: Ref,

        fn new(allocator: *Allocator, builder: *Ir.Program.Builder, returnType: Type, returnValue: ?Ref) Ref {
            const currentBlock = builder.getCurrentBasicBlock();

            if (!currentBlock.isTerminated) {
                const instruction = Instruction.new(allocator, builder, .ret, builder.instructions.ret.items.len);

                const ret = retBlk: {
                    if (returnValue) |value| {
                        builder.appendRef(value, instruction);
                        break :retBlk Ret{
                            .type = returnType,
                            .value = value,
                        };
                    } else {
                        break :retBlk Ret{
                            .type = returnType,
                            .value = Ref.Null,
                        };
                    }
                };

                builder.instructions.ret.append(ret) catch unreachable;
                return builder.appendInstruction2fn(instruction);
            } else {
                std.debug.panic("return is not allowed in terminated basic blocks", .{});
                // should prolly return undefined
            }
        }
    };

    pub const Br = struct {
        condition: ?Ref,
        destBasicBlock: u32,
        falsyDestBlock: ?u32,

        fn new(allocator: *Allocator, builder: *Ir.Program.Builder, destBasicBlock: u32) void {
            const currentBlock = builder.functionBuilders.items[builder.currentFunction].currentBlock;

            if (!currentBlock.isTerminated) {
                var list = &builder.instructions.br;
                list.append(.{
                    .condition = null,
                    .destBasicBlock = destBasicBlock,
                    .falsyDestBlock = null,
                }) catch unreachable;

                const instruction = Instruction.new(allocator, builder, .br, builder.instructions.br);
                builder.basicBlocks.items[destBasicBlock].refs.append(instruction) catch unreachable;
                builder.appendInstruction2fn(instruction);
            } else {
                std.debug.panic("branch is not allowed in terminated basic blocks", .{});
            }
        }

        pub fn newConditional(allocator: *Allocator, builder: *Ir.Program.Builder, condition: Ref, destBasicBlock: u32, falsyDestBlock: u32) void {
            const currentBlock = builder.functionBuilders.items[builder.currentFunction].currentBlock;

            if (!currentBlock.isTerminated) {
                var list = &builder.instructions.br;
                list.append(.{
                    .condition = condition,
                    .destBasicBlock = destBasicBlock,
                    .falsyDestBlock = falsyDestBlock,
                }) catch unreachable;

                const instruction = Instruction.new(allocator, builder, .br, list.items.len);

                builder.appendRef(condition, instruction);

                builder.basicBlocks.items[destBasicBlock].refs.append(instruction) catch unreachable;
                builder.basicBlocks.items[falsyDestBlock].refs.append(instruction) catch unreachable;
                builder.appendInstruction2fn(instruction);
            } else {
                std.debug.panic("branch is not allowed in terminated basic blocks", .{});
            }
        }
    };

    pub const Icmp = struct {
        left: Ref,
        right: Ref,
        id: Icmp.Id,

        pub const Id = enum(u8) {
            eq = 0,
            ne,
            ugt,
            uge,
            ult,
            ule,
            sgt,
            sge,
            slt,
            sle,
        };

        fn new(allocator: *Allocator, builder: *Ir.Program.Builder, id: Id, left: Ref, right: Ref) Ref {
            var list = &builder.instructions.icmp;
            list.append(.{
                .left = left,
                .right = right,
                .id = id,
            }) catch unreachable;

            const instruction = Instruction.new(allocator, builder, .icmp, list.items.len);
            builder.appendRef(left, instruction);
            builder.appendRef(right, instruction);
            return builder.appendInstruction2fn(instruction);
        }
    };

    pub const Alloc = struct {
        ptrType: Type,
        baseType: Type,
        ref: Ref,

        fn new(allocator: *Allocator, builder: *Ir.Program.Builder, refrence: Ref, allocType: Type, arraySize: ?*Ref) void {
            // assert that the arraySize is a null
            _ = arraySize;
            builder.instructions.alloc.append(.{
                .ptrType = builder.getOrCreatePtrType(allocType),
                .baseType = allocType,
                .ref = refrence,
            }) catch unreachable;
            const instruction = Instruction.new(allocator, builder, .alloc, builder.instructions.alloc.items.len);
            var fnBuilder = &builder.functionBuilders.items[builder.currentFunction];
            const entryIdx = fnBuilder.basicBlocs.items[0];

            builder.basicBlocks.items[entryIdx].instructions.insert(fnBuilder.nextAllocationIdx, instruction) catch unreachable;
            fnBuilder.nextAllocationIdx += 1;
            return instruction;
        }
    };

    pub const GetElPtr = struct {
        indices: []const i64,
        ptr: Ref,
        type: Type,

        pub fn new(allocator: *Allocator, builder: *Ir.Program.Builder, gepType: Type, ptr: Ref, indices: []const i64) Ref {
            var list = &builder.instructions.gep;
            list.append(.{
                .indices = indices,
                .ptr = ptr,
                .type = builder.getOrCreatePtrType(gepType),
            }) catch unreachable;
            const instruction = Instruction.new(allocator, builder, .getElPtr, list.items.len);
            builder.appendRef(ptr, instruction);
            return builder.appendInstruction2fn(instruction);
        }
    };
};
