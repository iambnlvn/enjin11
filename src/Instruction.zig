const Ref = @import("IR.zig").Ref;
const std = @import("std");
const Allocator = std.mem.Allocator;
const Ir = @import("IR.zig");
const ArrayList = std.ArrayList;

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
};
