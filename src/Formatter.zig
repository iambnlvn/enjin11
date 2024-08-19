const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const IR = @import("IR.zig");
const Program = IR.Program;
const Ref = IR.Ref;
const Constant = IR.Constant;
const BasicBlock = IR.BasicBlock;
const Function = IR.Function;
const Instruction = @import("Instruction.zig").Instruction;
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

            for (blockPrinters.items) |blockPrinter| {
                if (blockPrinter.id != 0xffffffff) {
                    try Format(writer, "{}:\n", .{blockPrinter.id}) catch unreachable;
                }

                for (blockPrinter.instructions.items) |instructionPrinter| {
                    const instructionRef = instructionPrinter.ref;
                    const instructionId = Instruction.getId();
                    const instructionStr = @tagName(instructionId);
                    const instructionIdx = instructionRef.getIDX();

                    if (instructionPrinter.id) |irId| {
                        try Format(writer, "\t%{} = ", .{irId});
                    } else {
                        try writer.writeAll("\t");
                    }
                    try Format(writer, "{s} ", .{instructionStr});

                    switch (instructionId) {
                        .call => {
                            const call = &self.builder.instructions.call.items[instructionIdx];
                            const calleeReturnType = call.type.toString(self);
                            const callee = call.callee;
                            const calleeDeclaration = switch (callee.getId()) {
                                .GlobalFunc => &self.builder.functionBuilders.items[callee.getIDX()].declaration,
                                .ExternalFunc => &self.builder.external.functions[callee.getIDX()].declaration,
                                else => std.debug.panic("Unknown Id ! {}", .{callee.getId()}),
                            };
                            try Format(writer, "{s} @{s}(", .{ calleeReturnType, calleeDeclaration.name });

                            for (call.args, 0..) |arg, argIdx| {
                                try self.formatRef(writer, func, arg, &slotTracker, calleeDeclaration.type.argTypes[argIdx]);
                                if (argIdx < call.args.len - 1) {
                                    try writer.writeAll(", ");
                                }
                            }
                            try writer.writeAll(")");
                        },
                        .alloc => {
                            const allocation = &self.builder.instructions.alloc.items[instructionIdx];
                            try Format(writer, "{s}", .{allocation.baseType.toString(self)});
                        },
                        .store => {
                            const store = &self.builder.instructions.store.items[instructionIdx];

                            try self.formatRef(writer, func, store.value, &slotTracker, store.type);
                            try writer.writeAll(", ");

                            try self.formatRef(writer, func, store.pointer, &slotTracker, null);
                        },
                        .br => {
                            const br = &self.builder.instructions.br.items[instructionIdx];
                            if (br.condition) |condition| {
                                try self.formatRef(writer, func, condition, &slotTracker, null);
                                try writer.writeAll(", ");
                                try self.formatRef(writer, func, BasicBlock.getRef(br.destBasicBlock), &slotTracker, null);
                                try writer.writeAll(", ");
                                try self.formatRef(writer, func, BasicBlock.getRef(br.falsyDestBlock.?), &slotTracker, null);
                            } else {
                                try self.formatRef(writer, func, BasicBlock.getRef(br.destBasicBlock), &slotTracker, null);
                            }
                        },
                        .load => {
                            const load = &self.builder.instructions.load.items[instructionRef.getIDX()];
                            try format(writer, "{s}, ", .{load.type.toString(self)});
                            try self.formatRef(writer, func, load.pointer, &slotTracker, load.type);
                        },
                        .icmp => {
                            const icmp = &self.builder.instructions.icmp.items[instructionRef.getIDX()];
                            try Format(writer, "{s}", .{@tagName(icmp.id)});
                            try self.formatRef(writer, func, icmp.left, &slotTracker, null);
                            try writer.writeAll(", ");
                            try self.formatRef(writer, func, icmp.right, &slotTracker, null);
                        },
                        .ret => {
                            const ret = &self.builder.instructions.ret.items[instructionRef.getIDX()];
                            try self.formatRef(writer, func, ret.value, &slotTracker, ret.type);
                        },
                        .memCopy => {
                            const memCopy = &self.builder.instructions.memCopy.items[instructionRef.getIDX()];
                            try self.formatRef(writer, func, memCopy.destination, &slotTracker, null);
                            try writer.writeAll(", ");

                            try self.formatRef(writer, func, memCopy.source, &slotTracker, null);
                            try writer.writeAll(", ");
                            try Format(writer, "i64 {}", .{memCopy.size});
                        },
                        .getElPtr => {
                            const gep = &self.builder.instructions.gep.items[instructionRef.getIDX()];
                            try Format(writer, "inbounds {s}, ", .{gep.type.toString(self)});
                            try self.formatRef(writer, func, gep.ptr, &slotTracker, null);
                            try writer.writeAll(", ");
                            for (gep.indices[0 .. gep.indices.len - 1]) |idx| {
                                try Format(writer, "{}, ", .{idx});
                            }
                            try Format(writer, "{}", .{gep.indices[gep.indices.len - 1]});
                        },
                        .add => {
                            const add = &self.builder.instructions.add.items[instructionRef.getIDX()];
                            try self.formatRef(writer, func, add.left, &slotTracker, null);
                            try writer.writeAll(", ");
                            try self.formatRef(writer, func, add.right, &slotTracker, null);
                        },
                        .sub => {
                            const sub = &self.builder.instructions.sub.items[instructionRef.getIDX()];
                            try self.formatRef(writer, func, sub.left, &slotTracker, null);
                            try writer.writeAll(", ");
                            try self.formatRef(writer, func, sub.right, &slotTracker, null);
                        },
                        .mul => {
                            const mul = &self.builder.instructions.mul.items[instructionRef.getIDX()];
                            try self.formatRef(writer, func, mul.left, &slotTracker, null);
                            try writer.writeAll(", ");
                            try self.formatRef(writer, func, mul.right, &slotTracker, null);
                        },
                        else => {
                            try Format(writer, "not implemented: {}\n", .{instructionId});
                            unreachable;
                        },
                    }
                    try writer.writeAll("\n");
                }
            }
            try writer.writeAll("}\n");
        }
    }

    fn formatRef(self: *const Formatter, writer: anytype, currentFn: *Function.builder, ref: Ref, slotTracker: *SlotTracker, expectedType: ?Type) !void {
        switch (ref.getId()) {
            .Constant => {
                switch (Constant.getId(ref)) {
                    .Array => {
                        try writer.writeAll("@arrayLitPlacerholder");
                    },
                    .@"struct" => {
                        const literal = self.builder.structLiterals.items[ref.getIDX()];
                        const refType = literal.type;
                        try Format(writer, "{s} @structLiteralPlaceholder", .{refType.toString(self)});
                    },
                    .Int => {
                        const typeStr = if (expectedType) |expType| expType.toString(self) else "s32";
                        const literal = self.builder.integerLiterals.items[ref.getIDX()];
                        try Format(writer, "{s} {}", .{ typeStr, literal.value });
                    },
                    else => std.debug.panic("Unknown or invalid constant Id {}", .{Constant.getId()}),
                }
            },
            .Instruction => {
                const idx = getIdx(writer, ref, slotTracker);
                switch (Instruction.getId(ref)) {
                    .alloc => {
                        const allocation = &self.builder.instructions.alloc.items[ref.getIDX()];
                        const allocTypeStr = allocation.ptrType.toString(self);

                        try Format(writer, "{s} %{}", .{ allocTypeStr, idx });
                    },
                    .load => {
                        const load = &self.builder.instructions.load.items[ref.getIDX()];
                        const loadTypeStr = load.type.toString(self);

                        try Format(writer, "{s} %{}", .{ loadTypeStr, idx });
                    },
                    .icmp => try Format(writer, "i1 %{}", .{idx}),
                    .mul, .add, .sub => try Format(writer, "s32 %{}", .{idx}),
                    .call => {
                        const call = &self.builder.instructions.call.items[ref.getIDX()];
                        try Format(writer, "{s} %{}", .{ call.type.toString(self), idx });
                    },
                    .getElPtr => {
                        const gep = &self.builder.instructions.gep.items[ref.getIDX()];
                        try Format(writer, "{s} %{}", .{ gep.type.toString(self), idx });
                    },
                    else => std.debug.panic("Unknown or invalid instruction id {}", .{Instruction.getId(ref)}),
                }
            },
            .BasicBlock => try Format(writer, "label %{}", .{getIdx(writer, ref, slotTracker)}),
            .arg => {
                const idx = ref.getIDX();
                const argType = currentFn.declaration.type.argTypes[idx];
                try Format(writer, "{s} %{}", .{ argType.toString(self), idx });
            },
            .None => try writer.writeAll("void"),
            else => std.debug.panic("Invalid id {}", .{ref.getId()}),
        }
    }
};
