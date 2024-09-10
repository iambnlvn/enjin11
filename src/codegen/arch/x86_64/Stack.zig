const std = @import("std");
const IR = @import("./../../../IR.zig");
const ArrayList = std.ArrayList;
const reg = @import("Register.zig");
const Register = reg.Register;
const Direction = reg.Direct;
const Indirect = reg.Indirect;

pub const Stack = struct {
    const Store = struct {
        const arrType = [Register.count]Stack.Store;
    };

    const Allocator = struct {
        allocations: ArrayList(Indirect),
        offset: i32,

        const Self = @This();

        fn new(allocator: *std.mem.Allocator) Stack.Allocator {
            return .{
                .allocations = ArrayList(Indirect).init(allocator.*),
                .offset = 0,
            };
        }

        fn allocate(self: *Self, size: u32, alloc: IR.Ref) void {
            const allocationOffset = @as(i32, std.mem.alignForward(@as(u32, @intCast(self.offset)), size));

            self.offset += @as(i32, @intCast(size));
            self.allocations.append(Indirect{
                .alignment = size,
                .offset = allocationOffset,
                .size = size,
                .ref = alloc,
                .register = stackReg,
            }) catch unreachable;
        }
    };
};

var stackReg: Register.ID = undefined;
// TODO: figure out how to expose IR.zig to the codegen module
// test "Stack Allocator new" {
//     var gpa = std.heap.GeneralPurposeAllocator(.{}){};
//     defer gpa.deinit();

//     const allocator = Stack.Allocator.new(&gpa.allocator);

//     // Check that the allocations field is initialized correctly
//     try std.testing.expect(allocator.allocations.items.len == 0);

//     // Check that the offset field is set to 0
//     try std.testing.expect(allocator.offset == 0);
// }
