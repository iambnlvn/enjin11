const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const IR = @import("IR.zig");
const Program = IR.Program;
const Ref = IR.Ref;
const Instruction = @import("Instruction.zig");
const Type = @import("Type.zig");
const Format = std.fmt.format;

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
        const formatter = Formatter{
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

    fn getIdx(writer: anytype, ref: Ref, slotTracker: *SlotTracker) u32 {
        for (slotTracker.slots.items, 0..) |slot, idx| {
            if (slot.value == ref.value) {
                return @as(u32, idx);
            }
        }
        Format(writer, "Failed to find slot for ref: {d}\n", .{ref}) catch unreachable;
        unreachable;
    }

    fn format(self: *const Formatter, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
        _ = fmt;

        for (self.builder.functionBuilders.items, 0..) |*func, funcIdx| {
            var slotTracker = SlotTracker{
                .slots = ArrayList(Ref).init(self.allocator),
                .startId = 0,
                .nextId = 0,
            };

            for (func.declaration.argNames, 0..) |_, argIdx| {
                slotTracker.newIdx(IR.Function.Arg.new(@as(u32, argIdx)));
            }
            slotTracker.newIdx(undefined);

            const basicBlockCount: u64 = func.basicBlocs.items.len;
            var blockPrinters = ArrayList(BlockPrinter).initCapacity(self.allocator, basicBlockCount) catch unreachable;

            const entryBlockIdx = func.basicBlocs.items[0];
            try self.setupBlock(&blockPrinters, &slotTracker, entryBlockIdx, 0xffffffff);

            if (basicBlockCount > 1) {
                for (func.basicBlocks.items[1..]) |basicBlockIdx| {
                    const blockId = slotTracker.newIdx(IR.BasicBlock.getRef(basicBlockIdx));
                    try self.setupBlock(&blockPrinters, &slotTracker, basicBlockIdx, blockId);
                }
            }

            try Format(writer, "\ndefine {s} @{s}(", .{ func.declaration.type.returnType.toString(self), func.declaration.name }) catch unreachable;

            const argCount = func.declaration.type.argTypes.len;

            if (argCount > 0) {
                for (func.declaration.type.argTypes[0 .. argCount - 1], 0..) |argType, argIdx| {
                    try Format(writer, "{s} %{}, ", .{ argType.toString(self), argIdx });
                }

                try Format(writer, "{s} %{}", .{ func.declaration.type.argTypes[argCount - 1].toString(self), argCount - 1 });
            }

            try Format(writer, ") #{}\n{c}\n", .{ funcIdx, '{' }) catch unreachable;

            // TODO: Implement block printing
            // for (blockPrinters.items) |blockPrinter| {
            //     if (blockPrinter.id != 0xffffffff) {
            //         try Format(writer, "{}:\n", .{blockPrinter.id}) catch unreachable;
            //     }

            //     for (blockPrinter.instructions.items) |instructionPrinter| {
            //         const instructionRef = instructionPrinter.ref;
            //     }
            // }
            try writer.writeAll("}\n");
        }
    }
};
