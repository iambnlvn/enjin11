const std = @import("std");
const ArrayList = std.ArrayList;
const Instruction = @import("./genInstruction.zig").GenInstruction;
const Register = @import("./Register.zig").Register;
const Stack = @import("./Stack.zig").Stack;

const BackwardPatch = struct {
    codeBufferOffset: u32,
};
pub const Program = struct {
    fns: []Function,
    dataBuffer: ArrayList(u8),

    const Function = struct {
        instructions: ArrayList(Instruction),
        basicBlockInstructionOffsets: ArrayList(u64),
        basicBlockBufferOffsets: ArrayList(u64),
        prevPatches: ArrayList(BackwardPatch),
        codeBufferOffset: u32,
        maxCallparamSize: i32,
        terminator: Terminator,
        regAllocator: Register.Allocator,
        stackAllocator: Stack.Allocator,

        const Terminator = enum {
            noreturn,
            ret,
        };

        fn appendInstruction(self: *Function, instr: Instruction) void {
            self.instructions.append(instr) catch unreachable;
        }
    };
};
