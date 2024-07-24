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
    const arrayIdEnumType = std.meta.Int(.unsigned, Level.position - arrayIdPosition);

    const LevelToArrayIdMap = blk: {
        const levels = std.enums.values(Level);
        var arrayIds: [levels.len]type = undefined;
        for (levels, 0..) |level, idx| {
            arrayIds[idx] = switch (level) {
                Level.builtin => Builtin,
                Level.module => Module,
                Level.global => Global,
                Level.scope => Scope,
            };
        }
        break :blk arrayIds;
    };
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
    pub fn getArrayId(self: Self, comptime level: Level) LevelToArrayIdMap[@intFromEnum(level)] {
        if (level != .scope and self.getLevel() != .global) {
            return @as(LevelToArrayIdMap[@intFromEnum(level)], @enumFromInt(@as(std.meta.Int(.unsigned, @bitSizeOf(LevelToArrayIdMap[@intFromEnum(level)])), @intCast((self.value & (std.math.maxInt(arrayIdEnumType) << arrayIdPosition)) >> arrayIdPosition))));
        }
    }
    pub fn getArrayIndex(self: Self) u32 {
        return @as(u32, @intCast((self.value & (std.math.maxInt(arrayIdEnumType) << arrayIdPosition)) >> arrayIdPosition));
    }
};
