const std = @import("std");
const Register = @import("Register.zig").Register;
const Instruction = @import("genInstruction.zig").GenInstruction;
const IrInstruction = @import("./../../../Instruction.zig").Instruction;

const sysVArgRegisters = [_]Register.ID{
    .DI,
    .SI,
    .C,
    .R8,
    .R9,
};

// microsoft visual c++ calling convention
const msvcArgRegisters = [_]Register.ID{
    .C, // stands for rcx
    .D, // stands for rdx
    .R8,
    .R9,
};

const OperandKind = enum {
    None,
    Immediate,
    Register,
    Stack,
    RelGlobal,
    RelExternal,
    RelLabel,
    RelDs,
};

// This setup is necessary for maintaining a consistent environment for function calls,
//  allowing the function to save the previous frame pointer and establish a new one.
// this ensures that local variables and function parameters are correctly managed.

// For setting the base pointer (rbp) to the current stack pointer (rsp). This
//  establishes a new stack frame for the function, making it easier to reference local
//  variables and function params
const movRBP_RSPBytes = [_]u8{ 0x48, 0x89, 0xe5 };

// Byte sequence for popping RBP
const popRBPBytes = [_]u8{0x5d};

// saves the current base pointer (rbp) onto the stack. This allows
// the function to restore the previous stack frame when it returns.
const pushRBPBytes = [_]u8{0x55};
// Byte sequence for returning from a function
const retBytes = [_]u8{0xc3};

const sysV_ABI_PrologueBytes = pushRBPBytes ++ movRBP_RSPBytes;

//  For generating the machine code for adjusting the stack pointer by a small constant value
fn addRsp_S8(n: i8) [4]u8 {
    var b = [_]u8{ 0x48, 0x83, 0xc4, undefined }; // not sure if I should put 0 or undefined for the last value
    @as(*i8, @ptrCast(&b[3])).* = n;
    return b;
}

fn addRsp_s32(n: i32) [7]u8 {
    var b = [_]u8{ 0x48, 0x81, 0xc4, undefined, undefined, undefined, undefined };

    @as(*align(1) i32, @ptrCast(&b[3])).* = n;
    return b;
}

fn callRel32(opIdx: u32) Instruction {
    const b = [_]u8{0xe8};

    return Instruction.Unresolved.new(b[0..], opIdx, .RelGlobal, 4, b.len);
}

fn callRip_Rel32(opIdx: u32) Instruction {
    const b = [_]u8{ 0x48, 0xff, 0x15 };

    return Instruction.Unresolved.new(b[0..], opIdx, .RelExternal, 4, b.len);
}

fn jccRel32(jmp: JumpOpcode, opIdx: u32) Instruction {
    const b = [_]u8{ 0x0f, @intFromEnum(jmp) + 0x10 };

    return Instruction.Unresolved.new(b[0..], opIdx, .RelLabel, 4, b.len);
}

fn jmpRel32(opIdx: u32) Instruction {
    const b = [_]u8{0xe9};

    return Instruction.Unresolved.new(b[0..], opIdx, .RelLabel, 4, b.len);
}
const JumpOpcode = enum(u8) {
    jge = 0x7d,
    jle = 0x7e,
    jne = 0x75,

    pub inline fn get(compId: IrInstruction.Icmp.Id) JumpOpcode {
        return switch (compId) {
            .slt => return .jge,
            .sgt => return .jle,
            .eq => .jne,
            else => unreachable,
        };
    }
};
