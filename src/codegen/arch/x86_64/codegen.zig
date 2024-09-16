const std = @import("std");
const panic = std.debug.panic;
const _reg = @import("Register.zig");
const Register = _reg.Register;
const Indirect = _reg.Indirect;
const Instruction = @import("genInstruction.zig").GenInstruction;
const IrInstruction = @import("./../../../Instruction.zig").Instruction;
const Parser = @import("./../../../Parser.zig");
const IR = @import("./../../../IR.zig");
const _prog = @import("./Program.zig");
const Program = _prog.Program;
const BackWardPatch = _prog.BackwardPatch;
const Stack = @import("Stack.zig").Stack;

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

//  For generating the machine code for adjusting the stack pointer by a small constant value
pub fn addRsp_S8(n: i8) [4]u8 {
    var b = [_]u8{ 0x48, 0x83, 0xc4, undefined }; // not sure if I should put 0 or undefined for the last value
    @as(*i8, @ptrCast(&b[3])).* = n;
    return b;
}

pub fn addRsp_s32(n: i32) [7]u8 {
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

pub fn subRsp_S8(n: i8) [4]u8 {
    var b = [_]u8{ 0x48, 0x83, 0xec, undefined };
    @as(*i8, @ptrCast(&b[3])).* = n;
    return b;
}

pub fn subRsp_s32(n: i32) [7]u8 {
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
            panic("64-bit not supported", .{});
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

fn addRegIndirect(dst: Register.ID, size: u8, indReg: Register.ID, indOff: i32) Instruction {
    var indOffsetByteCount: u8 = 0;

    if (indOffsetByteCount != 0) {
        indOffsetByteCount = if (indOff > std.math.maxInt(i8)) 4 else 1;
    }

    var b: [15]u8 = undefined;
    const byteCount: u8 = @as(u8, @intCast(@intFromBool(indReg == .SP))) + indOffsetByteCount + 2;

    const opCode: u8 = if (size == 1) 0x02 else 0x03;
    b[0] = opCode;

    b[1] = blk: {
        const base: u8 = if (indOff != 0) 0x40 else 0x00;

        break :blk base | @intFromEnum(dst) << 3 | @intFromEnum(indReg);
    };

    var offset: u8 = 2;
    if (indReg == .SP) {
        b[offset] = 0x24;
        offset += 1;
    }

    if (indOffsetByteCount > 0) {
        for (std.mem.asBytes(&indOff)[0..indOffsetByteCount]) |indOffByte| {
            b[offset] = indOffByte;
            offset += 1;
        }
    }

    return Instruction.Resolved.new(b[0..byteCount]);
}

fn addRegReg(dst: Register.ID, secondReg: Register.ID, size: u8) Instruction {
    const opCode = 0x01 - @intFromBool(size == 1);
    const regB = 0xc0 | @as(u8, @intCast(@intFromEnum(secondReg) << 3)) | @intFromEnum(dst);
    const b = [_]u8{ opCode, regB };

    return Instruction.Resolved.new(b[0..]);
}

fn addRegImm(reg: Register.ID, size: u8, imm: Parser.IntegerLiteral) Instruction {
    const val = imm.value;
    const immByteCount: u8 = if (val > std.math.maxInt(u32)) 8 else if (val > std.math.maxInt(u16)) 4 else if (val > std.math.maxInt(u8) and size > 2) 4 else if (val > std.math.maxInt(u8)) 2 else 1;

    if (reg == .A and immByteCount == 1 and size == 1) {
        const opCode: u8 = 0x05 - @as(u8, @intFromBool(size == 1));
        const immB = std.mem.asBytes(&val)[0..immByteCount];
        const arr = [_][]const u8{ std.mem.asBytes(&opCode), immB };

        return Instruction.Resolved.new(arr[0..]);
    }

    const opCode: u8 = switch (immByteCount) {
        1 => 0x83,
        2 => 0x81,
        4 => 0x81,
        8 => 0x81,
        else => unreachable,
    };

    const regB = 0xc0 | @intFromEnum(reg);
    const bytePrologue = [_]u8{ opCode, regB };
    const immB = std.mem.asBytes(&val)[0..immByteCount];
    const arr = [_][]const u8{ bytePrologue[0..], immB };

    return Instruction.Resolved.new(arr[0..]);
}

fn subRegImm(reg: Register.ID, size: u8, imm: Parser.IntegerLiteral) Instruction {
    const val = imm.value;
    var immByteCount: u8 = if (val > std.math.maxInt(u32)) 8 else if (val > std.math.maxInt(u16)) 4 else if (val > std.math.maxInt(u8)) 2 else 1;

    if (reg == .A and !(immByteCount == 1 and immByteCount != size)) {
        const opCode: u8 = 0x2d - @as(u8, @intFromBool(size == 1));
        immByteCount = if (immByteCount == size) size else immByteCount;

        const immB = std.mem.asBytes(&val)[0..immByteCount];
        const arr = [_][]const u8{ std.mem.asBytes(&opCode), immB };

        return Instruction.Resolved.newComponent(arr[0..]);
    } else {
        if (immByteCount == 1) {
            if (size > immByteCount) {
                const opCode: u8 = 0x83;
                const regB = 0xe8 | @intFromEnum(reg);
                const bytePrologue = [_]u8{ opCode, regB };
                const immB = std.mem.asBytes(&val)[0..immByteCount];
                const arr = [_][]const u8{ bytePrologue[0..], immB };

                return Instruction.Resolved.newComponent(arr[0..]);
            } else {
                unreachable;
            }
        } else {
            unreachable;
        }
    }
}

//TODO: validate which implementation is correct
// fn createInstruction(opCode: u8, reg: Register.ID, val: u64, immByteCount: u8) Instruction {
//     const regB = 0xe8 | @intFromEnum(reg);
//     const bytePrologue = [_]u8{ opCode, regB };
//     const immB = std.mem.asBytes(&val)[0..immByteCount];
//     const arr = [_][]const u8{ bytePrologue[0..], immB };

//     return Instruction.Resolved.new(arr[0..]);
// }

// fn subRegImm(reg: Register.ID, size: u8, imm: Parser.IntegerLiteral) Instruction {
//     const val = imm.value;
//     var immByteCount: u8 = if (val > std.math.maxInt(u32)) 8 else if (val > std.math.maxInt(u16)) 4 else if (val > std.math.maxInt(u8)) 2 else 1;

//     if (reg == .A and !(immByteCount == 1 and immByteCount != size)) {
//         const opCode: u8 = 0x2d - @as(u8, @intFromBool(size == 1));
//         immByteCount = if (immByteCount == size) size else immByteCount;

//         const immB = std.mem.asBytes(&val)[0..immByteCount];
//         const arr = [_][]const u8{ std.mem.asBytes(&opCode), immB };

//         return Instruction.Resolved.new(arr[0..]);
//     } else {
//         if (immByteCount == 1) {
//             if (size > immByteCount) {
//                 return createInstruction(0x83, reg, val, immByteCount);
//             } else {
//                 unreachable;
//             }
//         } else {
//             const opCode: u8 = if (immByteCount == 4) 0x81 else 0x83;
//             return createInstruction(opCode, reg, val, immByteCount);
//         }
//     }
// }

fn incReg(reg: Register.ID, size: u8) Instruction {
    const opCode: u8 = 0xff - @as(u8, @intFromBool(size == 1));
    const regB = 0xc0 | @intFromEnum(reg);
    var b = [_]u8{ opCode, regB };

    return Instruction.Resolved.new(b[0..]);
}

fn decReg(reg: Register.ID, size: u8) Instruction {
    const opCode: u8 = 0xff - @as(u8, @intFromBool(size == 1));
    const regB = 0xc8 | @intFromEnum(reg);
    var b = [_]u8{ opCode, regB };

    return Instruction.Resolved.new(b[0..]);
}

fn imulRegInd(reg: Register.ID, indReg: Register.ID, indOffset: i32) Instruction {
    const opCode = [_]u8{ 0x0f, 0xaf };

    return encodeIndirectInstructionOpcode(null, opCode[0..], @intFromEnum(reg) << 3, indReg, indOffset, std.mem.zeroes([]const u8));
}

fn xorReg(reg: Register.ID, size: u8) Instruction {
    const mod = 0b11;
    const rm = @intFromEnum(reg);

    const b = switch (size) {
        1, 2 => unreachable,
        4 => blk: {
            const bytes = [_]u8{ 0x31, 0xc0 | (mod << 6) | ((@intFromEnum(reg) & 0b111) << 3) | (rm & 0b111) };
            break :blk bytes[0..];
        },
        8 => unreachable,
        else => unreachable,
    };

    return Instruction.Resolved.new(b);
}

pub const Rex = enum(u8) {
    None = 0,
    Rex = 0x40,
    B = 0x41,
    X = 0x42,
    R = 0x44,
    W = 0x48,
};

//used to fetch the value from memory based on a given pointer.
//It  returns an indirect reference to the value, which includes information
//about the register and offset where the value is stored.
pub fn fetchLoad(
    prog: *const IR.Program,
    func: *Program.Function,
    loadRef: IR.Ref,
    ldUse: IR.Ref,
    comptime isRegisterLoad: bool,
) Indirect {
    const load = prog.instructions.load[loadRef.getIDX()];
    const loadPtrId = IrInstruction.getId(load.pointer);

    switch (loadPtrId) {
        .alloc => {
            const ldUseId = IrInstruction.getId(ldUse);
            switch (ldUseId) {
                .store => {
                    const store = prog.instructions.store[ldUse.getIDX()];
                    const resSize = @as(u8, @intCast(store.type.getSizeResolved(prog)));
                    const allocation = func.stackAllocator.getAlloc(func, prog, load.pointer);

                    const regSize = @as(u8, @intCast(allocation.size));
                    const loadReg = func.regAllocator.allocateDirect(load.pointer, regSize);
                    func.regAllocator.free(loadReg.register);

                    const moveRegStack = moveRegIndirect(
                        loadReg.register,
                        @as(u8, allocation.size),
                        allocation.register,
                        allocation.offset,
                    );

                    func.appendInstruction(moveRegStack);
                    return func.regAllocator.allocateIndirect(loadRef, 0, resSize);
                },
                .load, .getElPtr, .icmp => return func.regAllocator.allocateIndirect(func, prog, load.pointer),
                else => panic("Invalid load use", .{}),
            }
        },
        .load => {
            if (false) {
                const stackIndirect = fetchLoad(prog, func, load.pointer, loadRef, isRegisterLoad);

                const indirectReg = func.regAllocator.allocateIndirect(loadRef, 0, @as(u8, @intCast(stackIndirect.size)));

                const movRegStack = moveRegIndirect(
                    indirectReg.register,
                    @as(u8, @intCast(indirectReg.size)),
                    stackIndirect.register,
                    stackIndirect.offset,
                );
                func.appendInstruction(movRegStack);

                return indirectReg;
            } else {
                const loadSize = @as(u8, @intCast(load.type.getSizeResolved(prog)));

                const stackIndirect = fetchLoad(prog, func, load.pointer, loadRef, isRegisterLoad);
                const register = func.regAllocator.allocateDirect(load.pointer, @as(u8, @intCast(stackIndirect.size)));
                const movRegStack = moveRegIndirect(
                    register.register,
                    @as(u8, @intCast(register.size)),
                    stackIndirect.register,
                    stackIndirect.offset,
                );
                func.appendInstruction(movRegStack);

                func.regAllocator.free(register.register);
                return func.regAllocator.allocateIndirect(loadRef, 0, loadSize);
            }
        },
        .getElPtr => {
            const gepIndirect = func.stackAllocator.processGEP(func, prog, load.pointer);
            _ = gepIndirect;
            //TODO: figure it out
        },
        else => {
            panic("Invalid load pointer", .{});
        },
    }
}

const Data = struct {
    offsets: [Kind.count]std.ArrayList(u32),
    buffer: std.ArrayList(u8),

    const Kind = enum {
        integer,
        array,
        structure,
        const count = std.enums.values(Kind).len;
    };
};

pub fn encode(
    allocator: *std.mem.Allocator,
    prog: *const IR.Program,
    execFIleName: []const u8,
    target: std.Target,
) void {
    var abi = target.abi;
    const os = target.os.tag;
    if (os == .windows) abi = .msvc;
    // this is probably bad as this may crash if the abi is not implemented, instead of giving an err back
    // const argRegisters: Register.ID, const stackRegister: Register.ID = switch (abi) {
    //     .msvc => .{ msvcArgRegisters[0..], Register.ID.SP },
    //     .gnu => .{ sysVArgRegisters[0..], Register.ID.BP },
    // };
    _ = execFIleName;
    const argRegisters = switch (abi) {
        .gnu => sysVArgRegisters[0..],
        .msvc => msvcArgRegisters[0..],
        else => panic("{any} not implemented", .{@tagName(abi)}),
    };

    const stackRegister = switch (abi) {
        .gnu => Register.ID.BP,
        .msvc => Register.ID.SP,
        else => panic("{any} not implemented", .{@tagName(abi)}),
    };

    const functionCount = prog.functions.len;
    var functions = std.ArrayList(Program.Function).initCapacity(allocator.*, functionCount) catch unreachable;

    var data = Data{
        .buffer = std.ArrayList(u8).init(allocator.*),
        .offsets = undefined,
    };

    data.offsets[@intFromEnum(Data.Kind.array)] = std.ArrayList(u32).initCapacity(allocator.*, prog.arrayLiterals.len) catch unreachable;

    for (prog.arrayLiterals) |arrayLit| {
        const arrayType = prog.arrayTypes[arrayLit.type.getIdx()];
        const elSize = arrayType.type.getSizeResolved(prog);
        const dataBufferOffset = @as(u32, @intCast(data.buffer.items.len));
        data.offsets[@intFromEnum(Data.Kind.array)].appendAssumeCapacity(dataBufferOffset);

        for (arrayLit.elements) |el| {
            const intLit = prog.integerLiterals[el.getIdx()];
            data.buffer.appendSlice(std.mem.asBytes(&intLit.value[0..elSize])) catch unreachable;
        }
    }

    data.offsets[@intFromEnum(Data.Kind.structure)] = std.ArrayList(u32).initCapacity(allocator.*, prog.structLiterals.len) catch unreachable;

    for (prog.structLiterals, 0..) |structLit, structLitIdx| {
        const structSize = structLit.type.getSize(prog);
        const dataBufferOffset = @as(u32, @intCast(data.buffer.items.len));
        //TODO: remove the _ prefix later, vars will be used later
        _ = structSize;
        _ = structLitIdx;
        data.offsets[@intFromEnum(Data.Kind.structure)].appendAssumeCapacity(dataBufferOffset);

        for (structLit.fields) |field| {
            const intLit = prog.integerLiterals[field.getIdx()];
            data.buffer.appendSlice(std.mem.asBytes(&intLit.value[0..field.size])) catch unreachable;
        }
    }

    for (prog.functions) |*func| {
        var stackAllocator = Stack.Allocator.new(allocator);
        var regAllocator = Register.Allocator.new(argRegisters);

        for (func.declaration.type.argTypes, 0..) |argType, argIdx| {
            const argSize = argType.getSizeResolved(prog);
            _ = regAllocator.allocateArg(@as(u32, @intCast(argIdx)), @as(u8, @intCast(argSize)));
        }

        const basicBlockCount = func.basicBlocs.len;

        const entryBlockIdx = func.basicBlocs[0];
        const entryBlock = prog.basicBlocks[entryBlockIdx];

        for (entryBlock.instructions.items) |instruction| {
            if (IrInstruction.getId(instruction) == .alloc) _ = stackAllocator.allocate(@as(u32, @intCast(instruction.getSize(prog))), instruction);
        }

        functions.append(.{
            .instructions = std.ArrayList(Instruction).init(allocator) catch unreachable,
            .basicBlockBufferOffsets = std.ArrayList(allocator, basicBlockCount).init(allocator) catch unreachable,
            .basicBlockInstructionOffsets = std.ArrayList(u64).initCapacity(allocator, basicBlockCount) catch unreachable,
            .maxCallparamSize = 0,
            .prevPatches = std.ArrayList(BackWardPatch).init(allocator),
            .terminator = .noreturn,
            .regAllocator = regAllocator,
            .stackAllocator = stackAllocator,
        }) catch unreachable;
    }

    for (prog.functions, 0..) |*irFunc, irFuncIdx| {
        var function = &functions.items[irFuncIdx];
        var occupiedRegCount: u32 = 0;
        const argCount = irFunc.declaration.type.argTypes.len;
        _ = argCount;
        for (function.regAllocator.state.occupation.array) |occupation| {
            occupiedRegCount += @intFromBool(occupation != .None);
        }

        for (irFunc.basicBlocs, 0..) |basicBlockIdx, basicBlockIter| {
            const basicBlock = prog.basicBlocks[basicBlockIdx];
            if (basicBlockIter != 0) function.regAllocator.reset();

            const instructionOffset = function.instructions.items.len;
            function.basicBlockInstructionOffsets.appendAssumeCapacity(instructionOffset);

            for (basicBlock.instructions.items) |instruction| {
                const instructionId = IrInstruction.getId(instruction);
                const instructionIdx = instruction.getIDX();
                _ = instructionIdx;
                switch (instructionId) {
                    .icmp => processIcmp(prog, function, instruction),
                    .add => processAdd(prog, function, instruction, stackRegister),
                    .memCopy => processMemCopy(prog, function, instruction),
                    .sub => processSub(prog, function, instruction, stackRegister),
                    // Todo: implement the rest of the instructions
                    // .mul => processMul(prog, function, instruction),
                    else => panic("Instruction not implemented", .{}),
                }
            }
        }
    }
}

//TODO: Refactor this function

fn processIcmp(prog: *const IR.Program, func: *Program.Function, ref: IR.Ref) void {
    const icmpInstruction = prog.instructions.icmp[ref.getIDX()];
    var leftOperandKind: OperandKind = undefined;
    var rightOperandKind: OperandKind = undefined;

    const leftOperandAllocation = blk: {
        switch (icmpInstruction.left.getId()) {
            .Instruction => {
                const instructionId = IrInstruction.getId(icmpInstruction.left);
                switch (instructionId) {
                    .load => {
                        leftOperandKind = .Stack;
                        break :blk fetchLoad(prog, func, icmpInstruction.left, ref, true);
                    },
                    else => panic("Unexpected instructionId in processIcmp: {any}", .{instructionId}),
                }
            },
            else => panic("Unexpected icmpInstruction.left in processIcmp: {any}", .{IR.Constant.getId(icmpInstruction.left)}),
        }
    };

    switch (icmpInstruction.right.getId()) {
        .Constant => {
            switch (IR.Constant.getId(icmpInstruction.right)) {
                .Int => {
                    rightOperandKind = .Immediate;
                    const intLiteral = prog.integerLiterals[icmpInstruction.right.getIDX()];

                    const cmp = cmpBlk: {
                        if (leftOperandKind == .Stack) break :cmpBlk cmpIndirectImmediate(
                            leftOperandAllocation.register,
                            leftOperandAllocation.offset,
                            @as(u8, @intCast(leftOperandAllocation.size)),
                            intLiteral,
                        ) else unreachable;
                    };

                    func.appendInstruction(cmp);
                },
                else => panic("Unexpected constant type in processIcmp: {}", .{IR.Constant.getId(icmpInstruction.right)}),
            }
        },
        .Instruction => {
            const instructionId = IrInstruction.getId(icmpInstruction.right);

            switch (instructionId) {
                .load => {
                    if (leftOperandKind == .Stack) {
                        const regSize = @as(u8, @intCast(leftOperandAllocation.size));
                        const loadReg = func.regAllocator.allocateDirect(icmpInstruction.right, regSize);

                        const moveRegStack = moveRegIndirect(
                            loadReg.register,
                            @as(u8, @intCast(leftOperandAllocation.size)),
                            leftOperandAllocation.register,
                            leftOperandAllocation.offset,
                        );

                        func.regAllocator.alterAllocDirect(loadReg.register, ref);
                        func.appendInstruction(moveRegStack);

                        const secondOp = fetchLoad(prog, func, icmpInstruction.right, ref, false);
                        const cmpRegStack = cmpRegisterIndirect(loadReg.register, regSize, secondOp.register, secondOp.size);
                        func.appendInstruction(cmpRegStack);
                    } else unreachable;
                },
                else => panic("Unexpected else branch in processIcmp: invalid icmpInstruction.left value", .{}),
            }
        },
        else => panic("Unexpected else branch in processIcmp: invalid icmpInstruction", .{}),
    }
}

fn processAdd(prog: *const IR.Program, func: *Program.Function, addRef: IR.Ref, stackReg: Register.ID) void {
    const addInstruction = prog.instructions.add[addRef.getIDX()];

    switch (addInstruction.left) {
        .Instruction => {
            const instructionId = IrInstruction.getId(addInstruction.left);
            switch (instructionId) {
                .load => {
                    const firstOpAllocation = fetchLoad(prog, func, addInstruction.left, addRef, true);

                    switch (addInstruction.right.getId()) {
                        .Constant => {
                            switch (IR.Constant.getId(addInstruction.right)) {
                                .Int => {
                                    const secondOpIntLiteral = prog.integerLiterals[addInstruction.right.getIDX()];
                                    if (secondOpIntLiteral.value == 0) return;

                                    const regSize = @as(u8, @intCast(firstOpAllocation.size));
                                    const loadReg = func.regAllocator.allocateDirect(addInstruction.left, regSize);
                                    const movRegInd = moveRegIndirect(loadReg.register, @as(u8, @intCast(firstOpAllocation.size)), firstOpAllocation.register, firstOpAllocation.offset);

                                    if (firstOpAllocation.register != stackReg) {
                                        func.regAllocator.free(firstOpAllocation.register);
                                    }

                                    func.regAllocator.alterAllocDirect(loadReg.register, addRef);
                                    func.appendInstruction(movRegInd);

                                    if (secondOpIntLiteral.value == 1) {
                                        func.appendInstruction(incReg(loadReg.register, regSize));
                                    } else {
                                        const addRegLit = addRegImm(loadReg.register, regSize, secondOpIntLiteral);
                                        func.appendInstruction(addRegLit);
                                    }
                                },
                                else => panic("Unexpected constant type in processAdd: {any}", .{IR.Constant.getId(addInstruction.right)}),
                            }
                        },
                        .Instruction => {
                            switch (IrInstruction.getId(addInstruction.right)) {
                                .load => {
                                    const secOpLoad = prog.instructions.load[addInstruction.right.getIDX()];
                                    const secOpAllocation = func.stackAllocator.getAlloc(func, prog, secOpLoad.pointer);

                                    const loadReg = func.regAllocator.allocateDirect(addInstruction.right, @as(u8, @intCast(firstOpAllocation.size)));
                                    const movRegStack = moveRegIndirect(loadReg.register, @as(u8, @intCast(firstOpAllocation.size)), firstOpAllocation.register, firstOpAllocation.offset);
                                    func.appendInstruction(movRegStack);

                                    const addRegStack = addRegIndirect(loadReg.register, @as(u8, @intCast(firstOpAllocation.size)), secOpAllocation.register, secOpAllocation.offset);
                                    func.regAllocator.alterAllocDirect(loadReg.register, addRef);
                                    func.appendInstruction(addRegStack);
                                },
                                .Call => {
                                    // TODO: figure it out
                                    unreachable;
                                },
                                else => panic("Unexpected branch in processAdd: invalid addInstruction.right value", .{}),
                            }
                        },
                        else => panic("Unexpected branch in processAdd: invalid addInstruction.right", .{}),
                    }
                },
                .Call => {
                    const returnedCallReg = func.regAllocator.fetchDirect(prog, addInstruction.left, addRef) orelse unreachable;

                    switch (addInstruction.right.getId()) {
                        .Constant => {
                            switch (IR.Constant.getId(addInstruction.right)) {
                                .Int => {
                                    const secOpIntLiteral = prog.integerLiterals[addInstruction.right.getIDX()];
                                    if (secOpIntLiteral.value == 0) return;

                                    if (secOpIntLiteral.value == 1) {
                                        func.appendInstruction(incReg(returnedCallReg.register, returnedCallReg.size));
                                    } else {
                                        const addRegLit = addRegImm(returnedCallReg.register, returnedCallReg.size, secOpIntLiteral);
                                        func.appendInstruction(addRegLit);
                                    }
                                },
                                else => panic("Unexpected constant type in processAdd: {any}", .{IR.Constant.getId(addInstruction.right)}),
                            }
                        },
                        else => panic("Unexpected branch in processAdd: invalid addInstruction.right", .{}),
                    }
                },
                .Mul => {
                    const returnedCallReg = func.regAllocator.fetchDirect(prog, addInstruction.left, addRef) orelse unreachable;
                    switch (addInstruction.right.getId()) {
                        .Constant => {
                            switch (IR.Constant.getId(addInstruction.right)) {
                                .Int => {
                                    const secOpIntLiteral = prog.integerLiterals[addInstruction.right.getIDX()];
                                    if (secOpIntLiteral.value == 0) return;

                                    if (secOpIntLiteral.value == 1) {
                                        func.appendInstruction(incReg(returnedCallReg.register, returnedCallReg.size));
                                    } else {
                                        const addRegLit = addRegImm(returnedCallReg.register, returnedCallReg.size, secOpIntLiteral);
                                        func.appendInstruction(addRegLit);
                                    }
                                },
                                else => panic("Unexpected constant type in processAdd: {any}", .{IR.Constant.getId(addInstruction.right)}),
                            }
                        },
                        .Instruction => {
                            const rightInstructionId = IrInstruction.getId(addInstruction.right);
                            switch (rightInstructionId) {
                                .mul => {
                                    const rightReg = func.regAllocator.fetchDirect(prog, addInstruction.right, addRef) orelse unreachable;
                                    func.regAllocator.free(returnedCallReg.register);

                                    const addMulMul = addRegReg(returnedCallReg.register, rightReg.register, returnedCallReg.size);
                                    func.appendInstruction(addMulMul);

                                    _ = func.regAllocator.allocateDirect(addRef, returnedCallReg.size);
                                },
                                else => panic("Unexpected branch in processAdd: invalid addInstruction.right value", .{}),
                            }
                        },
                        else => panic("Unexpected branch in processAdd: invalid addInstruction.right", .{}),
                    }
                },
                else => panic("Unexpected branch in processAdd: invalid addInstruction.left", .{}),
            }
        },
        else => panic("Unexpected branch in processAdd: invalid addInstruction", .{}),
    }
}

fn processMemCopy(prog: *const IR.Program, data: *Data, func: *Program.Function, instruction: IR.Ref) void {
    const memCopyInstruction = prog.instructions.memCopy[instruction.getIDX()];
    const memCopySize = @as(u8, memCopyInstruction.size);

    const sourceId = memCopyInstruction.source.getId();
    switch (sourceId) {
        .Constant => {
            const sourceConstId = IR.Constant.getId(memCopyInstruction.source);
            const idx = switch (sourceConstId) {
                .Array => @intFromEnum(Data.Kind.array),
                .@"struct" => @intFromEnum(Data.Kind.structure),
                else => panic("Unexpected sourceConstId in processMemCopy: {s}", .{@tagName(sourceConstId)}),
            };

            const dataBufferOffset = data.offsets[idx].items[memCopyInstruction.source.getIDX()];
            const temp = func.regAllocator.allocateDirect(memCopyInstruction.destination, memCopySize);

            const movDs2Reg = moveRegDsRel(temp.register, temp.size, dataBufferOffset);
            func.appendInstruction(movDs2Reg);

            func.regAllocator.free(temp.register);

            const stackAllocation = func.stackAllocator.getAlloc(func, prog, memCopyInstruction.destination);
            const movRegStack = moveIndirectReg(
                stackAllocation.register,
                stackAllocation.offset,
                @as(u8, @intCast(stackAllocation.size)),
                temp.register,
                temp.size,
            );

            func.appendInstruction(movRegStack);
        },
        else => panic("Unexpected sourceId in processMemCopy: {any}", .{sourceId}),
    }
}

fn processSub(prog: *const IR.Program, func: *Program.Function, ref: IR.Ref, stackReg: Register.ID) void {
    const subInstruction = prog.instructions.sub[ref.getIDX()];
    var shouldLoadFirstArg = false;
    var firstOpKind: OperandKind = undefined;
    _ = firstOpKind; // this should be used for moving the first operand to a register when processing instruction
    var secondOpKind: OperandKind = undefined;

    const firstOpAllocation = blk: {
        switch (subInstruction.left.getId()) {
            .Instruction => {
                const instructionId = IrInstruction.getId(subInstruction.left);
                switch (instructionId) {
                    .load => break :blk fetchLoad(prog, func, subInstruction.left, ref, true),
                    else => panic("Unexpected instructionId in processSub: {any}", .{instructionId}),
                }
            },
            else => panic("Unexpected subInstruction.left in processSub: {any}", .{IR.Constant.getId(subInstruction.left)}),
        }
    };
    switch (subInstruction.right.getId()) {
        .Constant => {
            switch (IR.Constant.getId(subInstruction.right)) {
                .Int => {
                    secondOpKind = .Immediate;

                    const intLit = prog.integerLiterals[subInstruction.right.getIDX()];

                    if (intLit.value == 0) return;

                    const regSize = @as(u8, @intCast(firstOpAllocation.size));
                    const loadReg = func.regAllocator.allocateDirect(subInstruction.right, regSize);
                    const movRegInd = moveRegIndirect(
                        loadReg.register,
                        @as(u8, @intCast(firstOpAllocation.size)),
                        firstOpAllocation.register,
                        firstOpAllocation.offset,
                    );

                    func.regAllocator.alterAllocDirect(loadReg.register, ref);
                    func.appendInstruction(movRegInd);
                    if (firstOpAllocation.register != stackReg) func.regAllocator.free(firstOpAllocation.register);

                    if (intLit.value == 1) func.appendInstruction(decReg(loadReg.register, regSize)) else {
                        const subRegLiteral = subRegImm(loadReg.register, regSize, intLit);
                        func.appendInstruction(subRegLiteral);
                    }
                },
                else => panic("Unexpected constant type in processSub: {any}", .{IR.Constant.getId(subInstruction.right)}),
            }
        },
        .Instruction => {
            switch (IrInstruction.getId(subInstruction.right)) {
                //Todo: figure it out later
                .load => unreachable,
                else => panic("Unexpected branch in processSub: invalid subInstruction.right value", .{}),
            }
        },
        else => panic("Unexpected branch in processSub: invalid subInstruction.right", .{}),
    }
    if (shouldLoadFirstArg) unreachable;
}
