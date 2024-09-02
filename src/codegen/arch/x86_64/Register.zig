const std = @import("std");
const IR = @import("./../../../IR.zig");

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

const Register = extern union {
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

            std.debug.panic("Couldn't allocate register", .{});
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

            std.debug.panic("No free registers", .{});
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

        //     std.debug.panic("No free registers", .{});
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
        //     std.debug.panic("No registers have been allocated", .{});
        // }

        fn allocateArg(self: *Self, argIdx: u32, size: u8) Direct {
            const targetRegister = self.argRegisters[argIdx];
            var occupation = &self.state.occupation.array[@intFromEnum(targetRegister)];

            if (occupation == .None) {
                occupation.* = .Direct;

                var reg = &self.state.registers.array[@intFromEnum(targetRegister)];
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

            std.debug.panic("Register already occupied or not ready for use", .{});
        }
    };
};
