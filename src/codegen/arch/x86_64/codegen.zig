const std = @import("std");
const Register = @import("Register.zig").Register;
const Instruction = @import("genInstruction.zig").GenInstruction;
const IrInstruction = @import("./../../../Instruction.zig").Instruction;
const Parser = @import("./../../../Parser.zig");

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

fn subRsp_S8(n: i8) [4]u8 {
    var b = [_]u8{ 0x48, 0x83, 0xec, undefined };
    @as(*i8, @ptrCast(&b[3])).* = n;
    return b;
}

fn subRsp_s32(n: i32) [7]u8 {
    var b = [_]u8{ 0x48, 0x81, 0xec, undefined, undefined, undefined, undefined };

    @as(*align(1) i32, @ptrCast(&b[3])).* = n;
    return b;
}

fn leaIndirect(reg: Register.ID, indirectReg: Register.ID, indirectOff: i32) Instruction {
    const rexB = 0x48;
    const opCode = 0x8d;
    const isIndirectOffsetZero = indirectOff == 0;
    const isIndirectOffset32Bit = indirectOff > std.math.maxInt(i8) and indirectOff < std.math.minInt(i8);
    const regByte: u8 = (@as(u8, @intFromBool(!isIndirectOffsetZero and isIndirectOffset32Bit)) << 7) | (@as(u8, @intFromBool(!isIndirectOffsetZero and !isIndirectOffset32Bit)) << 6) | (@intFromEnum(reg) << 3) | @intFromEnum(indirectReg);

    const sibByte = 0x24;
    const bytes = blk: {
        if (!isIndirectOffsetZero) {
            if (isIndirectOffset32Bit) {
                const indirectOffsetBytes = std.mem.asBytes(&indirectOff);

                if (indirectReg == .SP) {
                    break :blk &[_]u8{ rexB, opCode, regByte, sibByte, indirectOffsetBytes[0], indirectOffsetBytes[1], indirectOffsetBytes[2], indirectOffsetBytes[3] };
                } else {
                    break :blk &[_]u8{ rexB, opCode, regByte, indirectOffsetBytes[0], indirectOffsetBytes[1], indirectOffsetBytes[2], indirectOffsetBytes[3] };
                }
            } else {
                const indirectOffsetByte: i8 = @as(i8, @intCast(indirectOff));
                const indirectOffsetBytes = std.mem.asBytes(&indirectOffsetByte);

                if (indirectReg == .SP) {
                    break :blk [_]u8{ rexB, opCode, regByte, sibByte, indirectOffsetBytes[0] };
                } else {
                    break :blk [_]u8{ rexB, opCode, regByte, indirectOffsetBytes[0] };
                }
            }
        } else {
            if (indirectReg == .SP) {
                break :blk [_]u8{ rexB, opCode, regByte, sibByte };
            } else {
                break :blk [_]u8{ rexB, opCode, regByte };
            }
        }
    };
    return Instruction.Resolved.new(bytes);
}

fn cmpIndirectImmediate(indirectReg: Register.ID, indirectOff: i32, indirectSize: u8, intLiteral: Parser.IntegerLiteral) Instruction {
    const intByteCount: u8 = blk: {
        if (intLiteral.value > std.math.maxInt(u32)) {
            std.debug.panic("64-bit not supported", .{});
        }
        if (intLiteral.value > std.math.maxInt(u16)) break :blk 4;
        if (intLiteral.value > std.math.maxInt(u8)) {
            if (indirectSize == 2) break :blk 2 else break :blk 4;
        }
        break :blk 1;
    };

    const intBytes = std.mem.asBytes(&intLiteral.value)[0..intByteCount];
    const opCode = if (intByteCount > 1 or indirectSize == std.math.maxInt(u8)) &[_]u8{0x81} else &[_]u8{0x83};

    return encodeIndirectInstructionOpcode(null, opCode, 0x38, indirectReg, indirectOff, intBytes);
}

fn cmpRegisterIndirect(reg: Register.ID, regSize: u8, indirectReg: Register.ID, indirectOff: i32) Instruction {
    const opCode = [_]u8{0x3b - @as(u8, @intFromBool(regSize == 1))};

    return encodeIndirectInstructionOpcode(null, opCode[0..], @intFromEnum(reg) << 3, indirectReg, indirectOff, std.mem.zeroes([]const u8));
}

fn encodeIndirectInstructionOpcode(rex: ?u8, opcode: []const u8, regByteStart: u8, indirectReg: Register.ID, indirectOffset: i32, afterComBytes: []const u8) Instruction {
    var bytes: [Instruction.maxBytes]u8 = undefined;
    var encodedByteCount: u8 = 0;

    var indirectOffsetByteCount: u8 = undefined;
    const indirectOffsetI8 = @as(i8, @intCast(indirectOffset));

    const indirectOffsetBytes = blk: {
        if (indirectOffset < std.math.minInt(i8) or indirectOffset > std.math.maxInt(i8)) {
            indirectOffsetByteCount = 4;
            break :blk @as([*]u8, @ptrFromInt(@intFromPtr(&indirectOffset)))[0..indirectOffsetByteCount];
        } else {
            indirectOffsetByteCount = 1;
            break :blk std.mem.asBytes(@as(*align(1) const i8, @ptrCast(&indirectOffsetI8)))[0..];
        }
    };

    if (rex) |rexB| {
        bytes[encodedByteCount] = rexB;
        encodedByteCount += 1;
    }

    for (opcode) |opcodeB| {
        bytes[encodedByteCount] = opcodeB;
        encodedByteCount += 1;
    }

    const isIndirectOffsetZero = indirectOffset == 0;
    const indirectOffsetRegBytePart = (@as(u8, 0x40) * ((@as(u8, @intFromBool(indirectOffsetByteCount == 4)) << 1) | 1)) * @as(u8, @intFromBool(!isIndirectOffsetZero));

    const regByte = regByteStart | @intFromEnum(indirectReg) | indirectOffsetRegBytePart;

    bytes[encodedByteCount] = regByte;
    encodedByteCount += 1;

    if (indirectReg == .SP) {
        bytes[encodedByteCount] = 0x24;
        encodedByteCount += 1;
    }

    if (!isIndirectOffsetZero) {
        for (indirectOffsetBytes) |b| {
            bytes[encodedByteCount] = b;
            encodedByteCount += 1;
        }
    }

    for (afterComBytes) |b| {
        bytes[encodedByteCount] = b;
        encodedByteCount += 1;
    }

    return Instruction.Resolved.newBytes(bytes, encodedByteCount);
}

fn movRegisterLiteral(reg: Register.ID, num: u64, byteCount: u16) Instruction {
    const numberBytes = std.mem.asBytes(&num);

    const byteSlice = switch (byteCount) {
        1 => blk1: {
            const b = [_]u8{ 0xb0 + @intFromEnum(reg), @as(u8, @truncate(num)) };
            break :blk1 b[0..];
        },
        2 => blk2: {
            const b = [_]u8{ 0x66, 0xb8 + @intFromEnum(reg), numberBytes[0], numberBytes[1] };
            break :blk2 b[0..];
        },
        4 => blk4: {
            const b = [_]u8{ 0xb8 + @intFromEnum(reg), numberBytes[0], numberBytes[1], numberBytes[2], numberBytes[3] };
            break :blk4 b[0..];
        },
        8 => blk8: {
            const b = [_]u8{ 0x48, 0xb8 + @intFromEnum(reg), numberBytes[0], numberBytes[1], numberBytes[2], numberBytes[3], numberBytes[4], numberBytes[5], numberBytes[6], numberBytes[7] };
            break :blk8 b[0..];
        },
        else => unreachable,
    };

    return Instruction.Resolved.new(byteSlice);
}

fn movRegReg(dest: Register.ID, src: Register.ID, size: u8) Instruction {
    const opCode: u8 = 0x89 - @as(u8, @intFromBool(size == 1));
    const regByte: u8 = 0xc0 | (@intFromEnum(src) << 3) | @intFromEnum(dest);
    const b = [_]u8{ opCode, regByte };
    return Instruction.Resolved.new(b[0..]);
}

fn moveRegIndirect(dest: Register.ID, size: u8, indirectReg: Register.ID, offset: i32) Instruction {
    return switch (size) {
        1, 2 => unreachable,
        4 => encodeIndirectInstructionOpcode(null, &[_]u8{0x8b}, @intFromEnum(dest) << 3, indirectReg, offset, std.mem.zeroes([]const u8)),
        8 => encodeIndirectInstructionOpcode(0x48, &[_]u8{0x8b}, @intFromEnum(dest) << 3, indirectReg, offset, std.mem.zeroes([]const u8)),
        else => unreachable,
    };
}

fn moveIndirectReg(reg: Register.ID, offset: i32, size: u8, destReg: Register.ID, destSize: u8) Instruction {
    const opCode: []const u8 = if (size > 1) &[_]u8{0x89} else &[_]u8{0x88};
    const rex = if (destSize == 8) @intFromEnum(Rex.W) else null;

    return encodeIndirectInstructionOpcode(rex, opCode, @intFromEnum(destReg) << 3, reg, offset, std.mem.zeroes([]u8));
}

fn moveIndirectImmediateUnsigned(indirectReg: Register.ID, offset: i32, imm: u64, size: u16) Instruction {
    return encodeIndirectInstructionOpcode(null, &[_]u8{0xc7}, 0, indirectReg, offset, std.mem.asBytes(&imm)[0..size]);
}

fn moveRegDsRel(dst: Register.ID, size: u8, offset: u32) Instruction {
    const b = switch (size) {
        4 => &[_]u8{ 0x8b, @intFromEnum(dst) << 3 | 0x05 },
        8 => &[_]u8{ 0x48, 0x8b, @intFromEnum(dst) << 3 | 0x05 },
        else => unreachable,
    };
    return Instruction.Unresolved.new(b, offset, .RelDs, 4, @as(u8, @intCast(b.len)));
}

pub const Rex = enum(u8) {
    None = 0,
    Rex = 0x40,
    B = 0x41,
    X = 0x42,
    R = 0x44,
    W = 0x48,
};
