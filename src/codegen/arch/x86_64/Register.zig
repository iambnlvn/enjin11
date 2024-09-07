const std = @import("std");
const IR = @import("./../../../IR.zig");
const Instruction = @import("./../../../Instruction.zig").Instruction;
const panic = std.debug.panic;

const Indirect = struct {
    ref: IR.Ref,
    offset: i32,
    alignment: u32,
    size: u32,
    register: Register.ID,
};

const Direct = struct {
    ref: IR.Ref,
    size: u8,
    modifier: u8,
    register: Register.ID,
};

pub const Register = extern union {
    direct: Direct,
    indirect: Indirect,

    const count = 16;

    const ArrayType = [Register.count]Register;

    pub const ID = enum(u8) {
        A,
        C,
        D,
        B,
        SP,
        BP,
        SI,
        DI,
        R8,
        R9,
        R10,
        R11,
        R12,
        R13,
        R14,
        R15,

        // I don't know if this is a good idea, figuring it out later maybe
        // const RegisterFlags = packed struct {
        //     AH: u4 = 4,
        //     CH: u4 = 5,
        //     DH: u4 = 6,
        //     BH: u4 = 7,
        // };

        // pub const registerFlags = RegisterFlags{};

        pub const AH: u8 = 4;
        pub const CH: u8 = 5;
        pub const DH: u8 = 6;
        pub const BH: u8 = 7;

        pub const bitMask: u8 = 0b1000;
    };

    const occupationType = enum(u2) {
        None,
        Direct,
        Indirect,
    };

    const Occupation = extern union {
        byType: struct {
            legacy: [LegacyGPR.count]occupationType,
            specialized: [SpecializedRegister.count]occupationType,
            new: [newRegister.count]occupationType,
        },
        array: [Register.count]occupationType,
    };

    const LegacyGPR = enum(u2) {
        A,
        C,
        D,
        B,
        const count = std.enums.values(LegacyGPR).len;
    };

    const SpecializedRegister = enum(u3) {
        SP = 4,
        BP,
        SI,
        DI,

        const count = std.enums.values(SpecializedRegister).len;
    };

    const State = struct {
        occupation: Occupation,
        registers: Registers,
    };

    const Registers = extern union {
        byType: struct {
            legacy: [LegacyGPR.count]Register,
            specialized: [SpecializedRegister.count]Register,
            new: [newRegister.count]Register,
        },
        array: [Register.count]Register,
    };

    const newRegister = enum(u4) {
        R8 = 8,
        R9,
        R10,
        R11,
        R12,
        R13,
        R14,
        R15,
        const count = std.enums.values(newRegister).len;
    };

    const Saver = struct {
        state: State,
        savedRegisterCount: u8,
    };

    const AccessType = enum {
        General,
        Legacy,
        specialized,
        args,
    };

    const Allocator = struct {
        state: State,
        argRegisters: []const Register.ID,

        const Self = @This();

        fn new(argRegisters: []const Register.ID) Self {
            var res: Self = undefined;
            res.argReigsters = argRegisters;
            res.state.occupation.array = std.mem.zeroes([Register.count]occupationType);
            return res;
        }

        fn free(self: *Self, reg: Register.ID) void {
            self.state.occupation.array[@intFromEnum(reg)] = .None;
        }

        fn allocateDirect(self: *Self, value: IR.Ref, regSize: u8) Direct {
            const registers = &self.state.registers.byType.legacy;
            const occupation = &self.state.occupation.byType.legacy;

            for (occupation, 0..) |*regOccupation, idx| {
                if (regOccupation.* == .None) {
                    regOccupation.* = .Direct;

                    registers[idx].* = Register{
                        .direct = Direct{
                            .ref = value,
                            .size = regSize,
                            .modifier = 0,
                            .register = @as(Register.ID, @enumFromInt(idx)),
                        },
                    };
                    return registers[idx].direct;
                }
            }

            panic("Couldn't allocate register", .{});
        }

        fn allocateIndirect(self: *Self, ref: IR.Ref, offset: i32, size: u32) Indirect {
            const registers = &self.state.registers.byType.legacy;
            const occupation = &self.state.occupation.byType.legacy;

            for (occupation, 0..) |*regOccupation, idx| {
                if (regOccupation.* == .None) {
                    regOccupation.* = .Indirect;

                    registers[idx].* = Register{
                        .indirect = Indirect{
                            .ref = ref,
                            .offset = offset,
                            .alignment = size,
                            .size = size,
                            .register = @as(Register.ID, @enumFromInt(idx)),
                        },
                    };
                    return registers[idx].indirect;
                }
            }

            panic("No free registers", .{});
        }

        // This seems to be a good approach to allocate registers but too much code for a basic functionality
        // might check out how to pass functions as parameters in zig, if possible, I can pass a function to allocate a specific register

        // fn allocateRegister(self: *Self, ref: IR.Ref, size: u32, kind: Occupation, offset: ?i32) void {
        //     const registers = &self.state.registers.byType.legacy;
        //     const occupation = &self.state.occupation.byType.legacy;

        //     for (occupation, 0..) |*regOccupation, idx| {
        //         if (regOccupation.* == .None) {
        //             regOccupation.* = kind;
        //             const regID = @as(Register.ID, @enumFromInt(idx));

        //             if (kind == .Direct) {
        //                 registers[idx].* = Register{
        //                     .direct = Direct{
        //                         .ref = ref,
        //                         .size = @as(u8, size),
        //                         .modifier = 0,
        //                         .register = regID,
        //                     },
        //                 };
        //             } else {
        //                 registers[idx].* = Register{
        //                     .indirect = Indirect{
        //                         .ref = ref,
        //                         .offset = offset.?,
        //                         .alignment = size,
        //                         .size = size,
        //                         .register = regID,
        //                     },
        //                 };
        //             }
        //             return;
        //         }
        //     }

        //     panic("No free registers", .{});
        // }

        // fn allocateDirect(self: *Self, value: IR.Ref, regSize: u8) Direct {
        //     self.allocateRegister(value, regSize, .Direct, null);
        //     const registers = &self.state.registers.byType.legacy;
        //     return registers[self.getLastAllocatedIndex()].direct;
        // }

        // fn allocateIndirect(self: *Self, ref: IR.Ref, offset: i32, size: u32) Indirect {
        //     self.allocateRegister(ref, size, .Indirect, offset);
        //     const registers = &self.state.registers.byType.legacy;
        //     return registers[self.getLastAllocatedIndex()].indirect;
        // }

        // fn getLastAllocatedIndex(self: *Self) usize {
        //     const occupation = &self.state.occupation.byType.legacy;
        //     for (occupation, 0..) |*regOccupation, idx| {
        //         if (regOccupation.* != .None) {
        //             return idx;
        //         }
        //     }
        //     panic("No registers have been allocated", .{});
        // }

        fn allocateArg(self: *Self, argIdx: u32, size: u8) Direct {
            const targetRegister = self.argRegisters[argIdx];
            const occupation = &self.state.occupation.array[@intFromEnum(targetRegister)];

            if (occupation.* == .None) {
                occupation.* = .Direct;

                const reg = &self.state.registers.array[@intFromEnum(targetRegister)];
                reg.* = .{
                    .direct = Direct{
                        .ref = IR.Function.Arg.new(argIdx),
                        .size = size,
                        .modifier = 0,
                        .register = targetRegister,
                    },
                };

                return reg.direct;
            }

            panic("Register already occupied or not ready for use", .{});
        }

        fn allocateCallArg(self: *Self, argIdx: u32, arg: IR.Ref, size: u8) Direct {
            const targetRegister = self.argRegisters[argIdx];

            const occupation = &self.state.occupation.array[@intFromEnum(targetRegister)];

            if (occupation.* == .None) {
                occupation.* = .Direct;

                const reg = &self.state.registers.array[@intFromEnum(targetRegister)];
                reg.* = .{
                    .direct = Direct{
                        .ref = arg,
                        .size = size,
                        .modifier = 0,
                        .register = targetRegister,
                    },
                };

                return reg.direct;
            }

            panic("No arg registers available or ready", .{});
        }

        fn allocateReturn(self: *Self, ref: IR.Ref, size: u8) Direct {
            const regA = @intFromEnum(Register.ID.A);
            const occupation = &self.state.occupation.byType.legacy[regA];

            if (occupation.* == .None) {
                occupation.* = .Direct;

                const reg = &self.state.registers.byType.legacy[regA];
                reg.* = .{
                    .direct = Direct{
                        .ref = ref,
                        .size = size,
                        .modifier = 0,
                        .register = Register.ID.A,
                    },
                };

                return reg.direct;
            }
            panic("Register A is already occupied", .{});
        }

        fn fetchDirect(self: *Self, program: *IR.Program, ref: IR.Ref, use: IR.Ref) ?Direct {
            for (self.state.occupation.byType.legacy, 0..) |*occupation, idx| {
                if (occupation.* == .Direct) {
                    const reg = &self.state.registers.byType.legacy[idx];

                    if (reg.direct.ref.value == ref.value) {
                        const uses = program.getRefs(Instruction.getId(ref), ref.getIDX());

                        if (uses[uses.len - 1].value == use.value) {
                            occupation.* = .None;
                        }
                        return reg.direct;
                    }
                }
            }
            return null;
        }

        fn fetchArg(self: *Self, argIdx: u32) Direct {
            const argReg = self.argRegisters[argIdx];
            const occupation = self.state.occupation.array[@intFromEnum(argReg)];

            if (occupation == .Indirect or occupation == .None) {
                panic("Register {s} doesn't have a direct value", .{@tagName(argReg)});
            }

            const res = self.state.registers.array[@intFromEnum(argReg)];

            if (res.direct.ref.getId() != .Arg) panic("Register {s} doesn't have an argument value", .{@tagName(argReg)});
            if (res.direct.ref.getIDX() != argIdx) panic("Register {s} doesn't have the correct argument", .{@tagName(argReg)});

            return res.direct;
        }

        fn reset(self: *Self) void {
            @memset(self.state.occupation.array[0..], occupationType.None);
        }

        fn shouldSave(reg: Register.ID) bool {
            const regInt = @intFromEnum(reg);
            return regInt <= @intFromEnum(Register.ID.D) or (regInt >= @intFromEnum(Register.ID.R8) and regInt <= @intFromEnum(Register.ID.R11));
        }

        // Todo: Implement ways to alter the allocation
    };
};
