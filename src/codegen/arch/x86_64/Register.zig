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

    // TODO: Implement Allocations
};
