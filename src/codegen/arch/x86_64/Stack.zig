const std = @import("std");
const panic = std.debug.panic;
const IR = @import("./../../../IR.zig");
const ArrayList = std.ArrayList;
const reg = @import("Register.zig");
const Register = reg.Register;
const Direction = reg.Direct;
const Indirect = reg.Indirect;
const Program = @import("Program.zig").Program;
const IrInstruction = @import("./../../../Instruction.zig").Instruction;
const Type = @import("./../../../Type.zig");
const codegen = @import("codegen.zig");

pub const Stack = struct {
    const Store = struct {
        const arrType = [Register.count]Stack.Store;
    };

    pub const Allocator = struct {
        allocations: ArrayList(Indirect),
        offset: i32,

        const Self = @This();

        pub fn new(allocator: *std.mem.Allocator) Stack.Allocator {
            return .{
                .allocations = ArrayList(Indirect).init(allocator.*),
                .offset = 0,
            };
        }

        pub fn allocate(self: *Self, size: u32, alloc: IR.Ref) void {
            const allocationOffset = @as(i32, std.mem.alignForward(@as(u32, @intCast(self.offset)), size));

            self.offset += @as(i32, @intCast(size));
            self.allocations.append(Indirect{
                .alignment = size,
                .offset = allocationOffset,
                .size = size,
                .ref = alloc,
                .register = stackReg,
            }) catch unreachable;
        }

        fn getAlloc(
            self: *Self,
            func: *Program.Function,
            prog: *const IR.Program,
            ref: IR.Ref,
        ) Indirect {
            switch (IrInstruction.getId(ref)) {
                .alloc => {
                    for (self.allocations.items) |allocation| {
                        if (allocation.ref.value == ref.value) return allocation;
                    }
                },
                .getElPtr => return self.processGEP(func, prog, ref),
                else => unreachable,
            }
            panic("Alloc not found", .{});
        }

        pub fn processGEP(
            self: *Self,
            func: *Program.Function,
            prog: *const IR.Program,
            instruction: IR.Ref,
        ) Indirect {
            const gep = prog.instructions.gep[instruction.getIDX()];
            const gepIdx = gep.indices[0];

            const gepPtrInstructionId = instruction.getId(gep.ptr);

            switch (gepPtrInstructionId) {
                .alloc => {
                    const allocation = prog.instructions.alloc[gep.ptr.getIDX()];

                    const allocationType = allocation.baseType;
                    switch (allocationType.getId()) {
                        .Array => {
                            const arrType = prog.arrayTypes[allocationType.getIdx()];
                            const elTypeSize = @as(u32, @intCast(arrType.type.getSizeResolved(prog)));
                            const arrayOffset = @as(i32, @intCast(@as(i64, @intCast(elTypeSize)) * gepIdx));

                            for (self.allocations.items) |alloc| {
                                if (allocation.ref.value == gep.ptr.value) {
                                    var finalAlloc = alloc;
                                    finalAlloc.offset += arrayOffset;
                                    finalAlloc.alignment = elTypeSize;
                                    finalAlloc.size = elTypeSize;
                                    finalAlloc.ref = instruction;
                                    return finalAlloc;
                                }
                            }
                            panic("alloc not found\n", {});
                        },
                        else => panic("invalid id {any}", .{allocationType.getId()}),
                    }
                },
                .load => {
                    const load = prog.instructions.load[gep.ptr.getIDX()];
                    const structTypeRef = Type.Pointer.getBaseType(load.type, prog.pointerTypes);
                    const structType = prog.structTypes[structTypeRef.getIdx()];

                    var offsetFromStruct: u32 = 0;

                    for (structType.types[0..@as(u32, @intCast(gepIdx))]) |fieldType| {
                        offsetFromStruct += @as(u32, fieldType.getSizeResolved(prog));
                    }

                    const fieldSize = structType.types[@as(u32, @intCast(gepIdx))].getSizeResolved(prog);
                    var indirectWithOffset = codegen.fetchLoad(prog, func, gep.ptr, instruction, false);

                    indirectWithOffset.offset += @as(u32, offsetFromStruct);
                    indirectWithOffset.size = @as(u32, @intCast(fieldSize));
                    indirectWithOffset.alignment = @as(u32, @intCast(fieldSize));
                    indirectWithOffset.ref = instruction;
                    return indirectWithOffset;
                },
                else => panic("", .{}),
            }
        }
    };
};

var stackReg: Register.ID = undefined;
// TODO: figure out how to expose IR.zig to the codegen module
// test "Stack Allocator new" {
//     var gpa = std.heap.GeneralPurposeAllocator(.{}){};
//     defer gpa.deinit();

//     const allocator = Stack.Allocator.new(&gpa.allocator);

//     // Check that the allocations field is initialized correctly
//     try std.testing.expect(allocator.allocations.items.len == 0);

//     // Check that the offset field is set to 0
//     try std.testing.expect(allocator.offset == 0);
// }
