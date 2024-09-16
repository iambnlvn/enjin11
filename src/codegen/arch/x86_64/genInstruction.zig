const std = @import("std");

pub const GenInstruction = struct {
    const Self = @This();
    pub const maxBytes = 15;
    pub const maxBytesBeforeOpResolution = 3;
    resolution: Resolution,
    status: Resolution.status,

    const Resolution = union {
        resolved: Resolved,
        unresolved: Unresolved,
        const status = enum(u8) {
            resolved,
            unresolved,
        };
    };
    pub const Resolved = struct {
        bytes: [maxBytes]u8,
        size: u8,

        pub inline fn new(bytes: []const u8) GenInstruction {
            return .{
                .resolution = .{
                    .resolved = .{
                        .bytes = blk: {
                            var res: [maxBytes]u8 = undefined;
                            // @memcpy(res[0..], bytes);
                            std.mem.copyForwards(u8, res[0..], bytes);
                            break :blk res;
                        },
                        .size = @as(u8, @intCast(bytes.len)),
                    },
                },
                .status = .resolved,
            };
        }

        pub inline fn newBytes(b: [maxBytes]u8, byteCount: u8) GenInstruction {
            return .{
                .resolution = .{
                    .resolved = .{
                        .bytes = b,
                        .size = byteCount,
                    },
                },
                .status = .resolved,
            };
        }

        pub inline fn newComponent(byteSlices: []const []const u8) GenInstruction {
            var offset: u8 = 0;

            return .{
                .resolution = .{
                    .resolved = .{
                        .bytes = blk: {
                            var res: [maxBytes]u8 = undefined;
                            for (byteSlices) |b| {
                                @memcpy(res[offset..], b[0..]);
                                offset += @as(u8, @intCast(b.len));
                            }
                            break :blk res;
                        },
                        .size = offset,
                    },
                },
                .status = .resolved,
            };
        }
    };

    pub const Unresolved = struct {
        bytes: [maxBytesBeforeOpResolution]u8,
        unknownOp: UnknownOp,

        pub const UnknownOp = extern struct {
            idx: u32,
            size: u8,
            offset: u8,
            kind: Kind,

            pub const Kind = enum(u8) {
                Immediate,
                RelGlobal,
                RelExternal,
                RelLabel,
                RelDs,
            };
        };

        pub inline fn new(knownBytes: []const u8, opIdx: u32, opKind: UnknownOp.Kind, opSize: u8, opOffset: u8) GenInstruction {
            return .{
                .resolution = .{
                    .unresolved = .{
                        .bytes = blk: {
                            var res: [maxBytesBeforeOpResolution]u8 = undefined;
                            @memcpy(res[0..], knownBytes[0..]);
                            break :blk res;
                        },
                        .unknownOp = .{
                            .idx = opIdx,
                            .size = opSize,
                            .offset = opOffset,
                            .kind = opKind,
                        },
                    },
                },
                .status = .unresolved,
            };
        }
    };
};
