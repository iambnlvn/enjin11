const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const IR = @import("IR.zig");
const Program = IR.Program;
const Ref = IR.Ref;
const Instruction = IR.Instruction;
const Type = @import("Type.zig");

pub const Formatter = struct {
    allocator: *Allocator,
    builder: *Program.Builder,

    const SlotTracker = struct {
        slots: ArrayList(Ref),
        startId: u32,
        nextId: u32,

        fn newIdx(self: *SlotTracker, ref: Ref) u32 {
            const id = self.nextId;
            self.slots.append(ref) catch unreachable;
            self.nextId += 1;
            return id;
        }
    };

    const InstructionPrinter = struct {
        ref: Ref,
        blockRef: u32,
        id: ?u32,
    };

    const BlockPrinter = struct {
        instructions: ArrayList(InstructionPrinter),
        ref: Ref,
        parent: *ArrayList(BlockPrinter),
        id: u32,
    };

    fn new(allocator: *Allocator, builder: *Program.Builder) void {
        var formatter = Formatter{
            .allocator = allocator,
            .builder = builder,
        };

        std.debug.print("{s}\n", .{formatter});
    }

    fn setupBlock(self: *const Formatter, blockPrinters: *ArrayList(BlockPrinter), slotTracker: *SlotTracker, basicBlockIdx: u32, blockId: u32) !void {
        const basicBlock = &self.builder.basicBlocks.items[basicBlockIdx];
        const instructionCount = basicBlock.instructions.len;

        var blockPrinter = BlockPrinter{
            .instructions = ArrayList(InstructionPrinter).initCapacity(self.allocator, instructionCount) catch unreachable,
            .ref = basicBlockIdx,
            .parent = blockPrinters,
            .id = blockId,
        };

        for (basicBlock.instructions.items) |instruction| {
            var instructionPrinter = InstructionPrinter{
                .ref = instruction,
                .blockRef = basicBlockIdx,
                .id = null,
            };
            const instructionId = Instruction.getId(instruction);

            switch (instructionId) {
                .call => {
                    const call = self.builder.instructions.call.items[instruction.getIDX()];

                    if (call.type.value != Type.Builtin.voidType.value) {
                        instructionPrinter.id = slotTracker.newIdx(instruction);
                    }
                },
                .alloc, .load, .add, .sub.mul, .icmp, .getElPtr => instructionPrinter.id = slotTracker.newIdx(instruction),
                .memCopy, .ret, .store, .br => {},
                else => std.debug.print("Unhandled instruction: {d}\n", .{instructionId}),
            }

            blockPrinter.instructions.append(instructionPrinter) catch unreachable;
        }
        blockPrinters.append(blockPrinter) catch unreachable;
    }
};
