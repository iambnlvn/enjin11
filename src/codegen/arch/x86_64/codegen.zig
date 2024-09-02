const std = @import("std");
const Register = @import("Register.zig").Register;

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
