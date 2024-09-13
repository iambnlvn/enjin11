const std = @import("std");
const ArrayList = std.ArrayList;
const Instruction = @import("./genInstruction.zig").GenInstruction;
const Register = @import("./Register.zig").Register;
const Stack = @import("./Stack.zig").Stack;
const Allocator = std.mem.Allocator;
const Pe = @import("Pe.zig");
const Codegen = @import("codegen.zig");

// This setup is necessary for maintaining a consistent environment for function calls,
//  allowing the function to save the previous frame pointer and establish a new one.
// this ensures that local variables and function parameters are correctly managed.

// For setting the base pointer (rbp) to the current stack pointer (rsp). This
//  establishes a new stack frame for the function, making it easier to reference local
//  variables and function params
pub const movRBP_RSPBytes = [_]u8{ 0x48, 0x89, 0xe5 };

// Byte sequence for popping RBP
pub const popRBPBytes = [_]u8{0x5d};

// saves the current base pointer (rbp) onto the stack. This allows
// the function to restore the previous stack frame when it returns.
pub const pushRBPBytes = [_]u8{0x55};
// Byte sequence for returning from a function
pub const retBytes = [_]u8{0xc3};

pub const sysV_ABI_PrologueBytes = pushRBPBytes ++ movRBP_RSPBytes;

var abi: std.Target.Abi = undefined;

const BackwardPatch = struct {
    codeBufferOffset: u32,
};

pub const Program = struct {
    fns: []Function,
    dataBuffer: ArrayList(u8),

    pub const Function = struct {
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

        pub fn appendInstruction(self: *Function, instr: Instruction) void {
            self.instructions.append(instr) catch unreachable;
        }
    };

    fn estimateMaxCodeSize(self: *Program) u64 {
        var totalInstructionCount: u64 = 0;
        const approxFixedInstructionCountPerFunction: u64 = 5;
        for (self.fns) |func| {
            totalInstructionCount = approxFixedInstructionCountPerFunction + func.instructions.items.len;
        }
        return totalInstructionCount * Instruction.maxBytes;
    }

    pub fn encodePeTextSection(self: *Program, allocator: *Allocator, text: *Pe.Section, out: *Pe.Section.Text.EncodingOutput, offset: *Pe.Offset) void {
        out.patches = ArrayList(Pe.Patch).init(allocator.*);
        const approxCodeSize = std.mem.alignForward(self.estimateMaxCodeSize(), Pe.fileAlignment);
        text.buffer.ensureTotalCapacity(approxCodeSize) catch unreachable;

        for (self.fns, 0..) |*func, funcIdx| {
            func.codeBufferOffset = @as(u32, text.buffer.items.len);

            for (func.prevPatches.items) |patch| {
                const ptrWriter = @as(*align(1) i32, @ptrCast(&text.buffer.items[patch.codeBufferOffset]));
                ptrWriter.* = @as(i32, @intCast(@as(i64, @intCast(func.codeBufferOffset)) - @as(i64, @intCast(patch.codeBufferOffset + @sizeOf(i32)))));
            }

            const encodeFramePtr = true;
            var addRspEpilogue = false;

            if (encodeFramePtr) {
                switch (abi) {
                    .gnu => {
                        text.buffer.appendSlice(sysV_ABI_PrologueBytes[0..]) catch unreachable;
                    },
                    .msvc => {
                        const stackOffset = func.stackAllocator.offset;

                        if (stackOffset > std.math.maxInt(i8)) {
                            const subRSP = Codegen.subRsp_s32(stackOffset)[0..];
                            text.buffer.appendSlice(subRSP) catch unreachable;
                            addRspEpilogue = true;
                        } else if (stackOffset > 0) {
                            const subRSP = Codegen.subRsp_s8(stackOffset)[0..];
                            text.buffer.appendSlice(subRSP) catch unreachable;
                            addRspEpilogue = true;
                        }
                    },
                    else => std.debug.panic("{} not implemented\n", .{abi}),
                }
            }
            var currentBlockOffset: u32 = 0;
            const basicBlockCount = func.basicBlockInstructionOffsets.items.len;
            var postFuncPatches = ArrayList(ArrayList(u32)).initCapacity(allocator.*, basicBlockCount) catch unreachable;
            postFuncPatches.items.len = basicBlockCount;

            for (postFuncPatches.items) |*patch| {
                patch.* = ArrayList(u32).init(allocator.*) catch unreachable;
            }
            postFuncPatches = postFuncPatches.items;

            var postFuncPatchCount: u32 = 0;

            for (func.instructions.items, 0..) |instruction, idx| {
                const instructionBufferOffset = text.buffer.items.len;

                if (currentBlockOffset < func.basicBlockInstructionOffsets.items.len) {
                    const instructionOffsets = func.basicBlockInstructionOffsets.items[currentBlockOffset..];

                    for (instructionOffsets) |currentBlockInstructionOffset| {
                        if (currentBlockInstructionOffset > idx) break;
                        func.basicBlockBufferOffsets.append(instructionBufferOffset) catch unreachable;
                        currentBlockOffset += 1;
                    }
                }

                if (instruction.status == .resolved) {
                    text.buffer.appendSlice(instruction.resolution.resolved.bytes[0..instruction.resolution.resolved.size]) catch unreachable;
                } else {
                    const unresolved = instruction.resolution.unresolved;
                    const operand = unresolved.unknownOp;
                    const operandOffset = operand.offset;
                    const operandSize = operand.size;

                    const instructionLength = operandOffset + operandSize;
                    const source = text.buffer.items.len + instructionLength;
                    const codeBufferOffset = @as(u32, @intCast(text.buffer.items.len + operandOffset));
                    const knownBytes = unresolved.bytes[0..operandOffset];
                    text.buffer.appendSlice(knownBytes) catch unreachable;

                    const operandKind = operand.kind;

                    switch (operandKind) {
                        .RelGlobal => {
                            if (operand.idx <= funcIdx) {
                                const target = self.fns[operand.idx].codeBufferOffset;
                                const rel32 = @as(i32, @intCast(@as(i64, @intCast(target)) - @as(i64, @intCast(source))));
                                text.buffer.appendSlice(std.mem.asBytes(&rel32)) catch unreachable;
                            } else {
                                text.buffer.appendNTimes(0xcc, operandSize) catch unreachable;
                                self.fns[operand.idx].prevPatches.append(BackwardPatch{ .codeBufferOffset = codeBufferOffset }) catch unreachable;
                            }
                        },
                        .RelExternal => {
                            out.patches.append(.{
                                .sourceSectionBuffer = operand.idx,
                                .destinationSectionBuffer = codeBufferOffset,
                                .section2ReadFrom = .@".text",
                                .section2WriteTo = .@".rdata",
                            }) catch unreachable;

                            text.buffer.appendNTimes(0xcc, operandSize) catch unreachable;
                        },
                        .RelLabel => {
                            const basicBlockIdx = operand.idx;

                            if (basicBlockIdx < currentBlockOffset) {
                                const target = func.basicBlockBufferOffsets.items[basicBlockIdx];
                                const rel32 = @as(i32, @intCast(@as(i64, @intCast(target)) - @as(i64, @intCast(source))));
                                text.buffer.appendSlice(std.mem.asBytes(&rel32)) catch unreachable;
                            } else {
                                text.buffer.appendNTimes(0xcc, operandSize) catch unreachable;
                                postFuncPatches[basicBlockIdx].append(codeBufferOffset) catch unreachable;
                                postFuncPatchCount += 1;
                            }
                        },
                        .RelDs => {
                            out.patches.append(.{
                                .sourceSectionBuffer = operand.idx,
                                .destinationSectionBuffer = codeBufferOffset,
                                .section2ReadFrom = .@".data",
                                .section2WriteTo = .@".text",
                            }) catch unreachable;
                            text.buffer.appendNTimes(0xcc, operandSize) catch unreachable;
                        },
                        else => unresolved,
                    }
                }
            }

            if (currentBlockOffset < func.basicBlockInstructionOffsets.items.len) {
                func.basicBlockBufferOffsets.append(text.buffer.items.len) catch unreachable;
                currentBlockOffset += 1;
            }

            var resolutionCount: u32 = 0;

            for (func.basicBlockBufferOffsets.items, 0..) |blockBufferOffset, blockIdx| {
                for (postFuncPatches[blockIdx].items) |patchBufferIdx| {
                    resolutionCount += 1;

                    const ptrWriter = @as(*align(1) i32, @ptrCast(&text.buffer.items[patchBufferIdx]));
                    const value2Write = @as(i32, @intCast(@as(i64, @intCast(blockBufferOffset)) - @as(i64, @intCast(patchBufferIdx + @sizeOf(i32)))));
                    ptrWriter.* = value2Write;
                }
            }

            if (addRspEpilogue) {
                if (func.terminator == .ret) {
                    switch (abi) {
                        .gnu => {
                            unreachable;
                        },
                        .msvc => {
                            const stackOffset = func.stackAllocator.offset;

                            if (stackOffset > std.math.maxInt(i8)) {
                                text.buffer.appendSlice(Codegen.addRsp_s32(stackOffset)[0..]) catch unreachable;
                            } else if (stackOffset > 0) {
                                text.buffer.appendSlice(Codegen.addRsp_S8(@as(i8, @intCast(stackOffset)))[0..]) catch unreachable;
                            }
                        },
                        else => {
                            std.debug.panic("{} not implemented\n", .{abi});
                        },
                    }
                }
            }
            switch (func.terminator) {
                .ret => text.buffer.append(retBytes[0]) catch unreachable,
                .noreturn => text.buffer.append(0xcc) catch unreachable,
                else => unreachable,
            }
        }
        const codeSize = @as(u32, text.buffer.items.len);
        text.header.misc.virtualSize = codeSize;
        text.header.rowDataSize = @as(u32, std.mem.alignForward(codeSize, Pe.fileAlignment));
        offset.afterSize(text.header.rowDataSize);
    }
};
