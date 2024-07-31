const Ref = @import("IR.zig").Ref;
const std = @import("std");

pub const Instruction = struct {
    pub fn getId(reference: Ref) ID {
        return @as(ID, @enumFromInt(@as(u8, @intCast((reference.value & (std.math.maxInt(std.meta.Int(.unsigned, @bitSizeOf(ID))) << ID.position)) >> ID.position))));
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
};
