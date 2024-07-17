const std = @import("std");
const EntityID = @import("EntityID.zig").ID;
const Builtin = EntityID.Builtin;
const Global = EntityID.Global;
const Scope = EntityID.Scope;
const Module = EntityID.Module;
const Level = EntityID.Level;

pub const Entity = packed struct {
    const Self = @This();
    const Type = u64;
    value: Type,

    const arrayIdPosition = Level.position - @bitSizeOf(Level);
    const arrayIndexPosition = @bitSizeOf(u32);

    pub fn new(baseIdx: u64, comptime arrayId: anytype, arrayIdx: u64) Self {
        const level = comptime switch (@TypeOf(arrayId)) {
            Builtin => Level.builtin,
            Module => Level.module,
            Global => Level.global,
            Scope => Level.scope,
            else => unreachable,
        };
        return Self{
            .value = (@as(Self.Type, @intCast(@intFromEnum(level))) << Level.position) | (@as(Self.Type, @intCast(@intFromEnum(arrayId))) << arrayIdPosition) | (arrayIdx << arrayIndexPosition) | baseIdx,
        };
    }
};
