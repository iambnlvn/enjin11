const Module = @import("Module.zig").Module;
const std = @import("std");
const Allocator = std.mem.Allocator;
pub const AST = struct {
    modules: [*]Module,
    moduleNames: [*][]const u8,
    moduleDirs: [*][]const u8,
    moduleLen: u64,
    moduleCap: u64,

    const Self = @This();
    fn parse(allocator: *Allocator, sourceFileName: []const u8, target: std.Target) Self {
        const minModuleCount = 8;
        var ast = Self{
            .modules = (allocator.alloc(Module, minModuleCount) catch unreachable).ptr,
            .moduleNames = (allocator.alloc([]const u8, minModuleCount) catch unreachable).ptr,
            .moduleDirs = (allocator.alloc([]const u8, minModuleCount) catch unreachable).ptr,
            .moduleLen = minModuleCount,
            .moduleCap = minModuleCount,
        };
        //parse modules from sourceFileName depending on the target
        return ast;
    }
};
