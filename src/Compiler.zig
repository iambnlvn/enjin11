const std = @import("std");
const AST = @import("AST.zig").AST;
const IR = @import("IR.zig");
const Analyzer = @import("Analyzer.zig");
const Allocator = std.mem.Allocator;

pub fn MakeExec(pageAllocator: *Allocator, sourceFileName: []const u8, execFileName: []const u8, target: std.Target) void {
    var arena = std.heap.ArenaAllocator.init(pageAllocator.*);
    defer arena.deinit();
    const allocator = &arena.allocator;

    const ast = AST.parse(allocator.*, sourceFileName, target);
    const semantics = Analyzer.analyze(allocator.*, ast);
    const ir = IR.gen(allocator, semantics);
    // encode
}
