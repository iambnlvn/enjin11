const std = @import("std");
const Allocator = std.mem.Allocator;
const IR = @import("./../IR.zig");
const Arraylist = std.ArrayList;

pub fn encode(allocator: *Allocator, module: *const IR.Program, exeFileName: []const u8, target: std.Target) void {
    const arch = target.cpu.arch;
    _ = allocator;
    _ = module;
    _ = exeFileName;
    switch (arch) {
        .x86_64 => {
            // TODO: Implement x86_64 codegen
            // only x86_64 is supported for now
        },
        else => {
            @compileError("Unsupported architecture");
        },
    }
}

pub fn writeExeFile(fileName: []const u8, content: []const u8) void {
    const isUnix = std.os.linux or std.os.macOS;
    std.debug.print("Writing executable file: {}\n", .{fileName});
    const file = try std.fs.cwd().createFile(fileName, comptime if (isUnix) .{
        .mode = 0o777,
    });
    defer file.close();

    file.writeAll(content) catch |err| {
        std.debug.panic("Error writing file: {}\n", .{err});
    };
}
