const std = @import("std");
const Allocator = std.mem.Allocator;
const Arraylist = std.ArrayList;

const DirectoryIdx = enum {
    @"export",
    import,
    resource,
    exception,
    security,
    relocation,
    debug,
    arch,
    globalPtr,
    TLS,
    loadConfig,
    bound,
    IAT,
    delayImport,
    CLR,
};

const imageSize = 8;

pub const ImageSectionHeader = extern struct {
    name: [imageSize]u8,
    misc: extern union {
        phyicalAddress: u32,
        virtualSize: u32,
    },
    virtualAddress: u32,
    rowDataSize: u32,
    ptrToRawData: u32,
    ptrToRelocations: u32,
    ptrToLineNumbers: u32,
    relocationCount: u16,
    lineNumberCount: u16,
    caracs: u32,

    const Characteristics = enum(u32) {
        containsCode = 0x20,
        containsInitializedData = 0x40,
        containsUninitializedData = 0x80,
        linkInfo = 0x200,
        memoryDiscardable = 0x2000000,
        memoryNotCached = 0x4000000,
        memoryNotPaged = 0x8000000,
        memoryShared = 0x10000000,
        memoryExecute = 0x20000000,
        memoryRead = 0x40000000,
        memoryWrite = 0x80000000,
    };
};

pub const Section = struct {
    header: ImageSectionHeader,
    buffer: Arraylist(u8),
    const Index = enum(u8) {
        @".text",
        @".rdata",
        @".data",
    };
    const count = std.enums.values(Index).len;
    const NameType = [imageSize]u8;

    const names = blk: {
        var sectionNames: [Section.count]NameType = undefined;
        sectionNames[@intFromEnum(Section.Index.@".text")] = NameType{ '.', 't', 'e', 'x', 't', 0, 0, 0 };
        sectionNames[@intFromEnum(Section.Index.@".rdata")] = NameType{ '.', 'r', 'd', 'a', 't', 'a', 0, 0 };
        sectionNames[@intFromEnum(Section.Index.@".data")] = NameType{ '.', 'd', 'a', 't', 'a', 0, 0, 0 };
        break :blk sectionNames;
    };

    const caracteristics = blk: {
        var sectionCaracteristics: [Section.count]u32 = [_]u32{0} ** Section.count;
        sectionCaracteristics[@intFromEnum(Section.Index.@".text")] = @intFromEnum(ImageSectionHeader.Characteristics.containsCode) | @intFromEnum(ImageSectionHeader.Characteristics.memoryRead) | @intFromEnum(ImageSectionHeader.Characteristics.memoryExecute);
        sectionCaracteristics[@intFromEnum(Section.Index.@".rdata")] = @intFromEnum(ImageSectionHeader.Characteristics.containsInitializedData) | @intFromEnum(ImageSectionHeader.Characteristics.memoryRead);
        sectionCaracteristics[@intFromEnum(Section.Index.@".data")] = @intFromEnum(ImageSectionHeader.Characteristics.containsInitializedData) | @intFromEnum(ImageSectionHeader.Characteristics.memoryRead) | @intFromEnum(ImageSectionHeader.Characteristics.memoryWrite);
        break :blk sectionCaracteristics;
    };

    pub fn getRVA(self: *Section) u32 {
        return self.header.virtualAddress + @as(u32, self.buffer.items.len);
    }
    fn appendStr(self: *Section, str: []const u8) void {
        const len = str.len + 1;
        self.buffer.appendSlice(str) catch unreachable;
        self.buffer.append(0) catch unreachable;

        if (len & 1 != 0) self.buffer.append(0) catch unreachable;
    }
};
