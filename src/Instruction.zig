const Ref = @import("IR.zig").Ref;
const std = @import("std");
const Allocator = std.mem.Allocator;
const Ir = @import("IR.zig");

pub const Instruction = struct {
    pub fn getId(reference: Ref) ID {
        return @as(ID, @enumFromInt(@as(u8, @intCast((reference.value & (std.math.maxInt(std.meta.Int(.unsigned, @bitSizeOf(ID))) << ID.position)) >> ID.position))));
    }

    pub fn new(allocator: *Allocator, builder: *Ir.Program.Builder, id: ID, index: u64) Ref {
        // at this point I dont know how to implement this function
        //paper time I guess
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
            // ? I'm just free-styling here
        }
    };
};
