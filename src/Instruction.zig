const Ref = @import("IR.zig").Ref;
const std = @import("std");
const Allocator = std.mem.Allocator;
const Ir = @import("IR.zig");
const ArrayList = std.ArrayList;
const Type = @import("Type.zig").Type;
pub const Instruction = struct {
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
};
