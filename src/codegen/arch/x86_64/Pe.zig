const std = @import("std");
const Allocator = std.mem.Allocator;
const Arraylist = std.ArrayList;
const Semantics = @import("./../../../Sem.zig");
const Program = @import("Program.zig").Program;

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
pub const fileAlignment = 0x200;

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

const ImageImportDescriptor = extern struct {
    caracsOrOriginalFirstThunk: u32,
    timeDateStamp: u32,
    forwarderChain: u32,
    name: u32,
    firstThunk: u32,
};

pub const Section = struct {
    header: ImageSectionHeader,
    buffer: Arraylist(u8),
    const Index = enum(u8) {
        @".text",
        @".rdata",
        @".data",
    };

    const Directory = extern struct {
        RVA: u32,
        size: u32,
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

    const Rdata = struct {
        const EncodingOutput = struct {
            libOffsets: Arraylist(LibOffsets),
            iat: Section.Directory,
            import: Section.Directory,
        };
    };

    fn encodeRdataSection(allocator: *Allocator, external: Semantics.External, rDataOut: *Rdata.EncodingOutput, offset: *Offset) Section {
        var rData = Section{ .header = std.mem.zeroInit(ImageSectionHeader, ImageSectionHeader{
            .ptrToRawData = offset.file,
            .virtualAddress = offset.virtual,
            .name = Section.names[@intFromEnum(Section.Index.@".rdata")],
        }), .buffer = Arraylist(u8).init(allocator.*) };

        var pe32Libs = Arraylist(ImportedLib).initCapacity(allocator.*, external.libs.len) catch unreachable;

        for (external.libs) |lib| {
            var symRVAs = Arraylist(u32).initCapacity(allocator, lib.symbols.len) catch unreachable;

            for (lib.symbols) |idx| {
                symRVAs.appendAssumeCapacity(rData.getRVA()) catch unreachable;
                rData.buffer.appendSlice(std.mem.asBytes(&@as(u16, @intCast(0)))) catch unreachable;
                rData.appendStr(external.symNames[idx]);
            }

            pe32Libs.append(ImportedLib{
                .nameRVA = 0,
                .RVA = 0,
                .imgThunkRVA = 0,
                .symRVAs = symRVAs.items,
            }) catch unreachable;
        }

        const IATRVA = rData.getRVA();
        rDataOut.libOffsets = Arraylist(LibOffsets).initCapacity(allocator.*, pe32Libs.items.len) catch unreachable;

        for (pe32Libs.items) |*pe32Lib| {
            pe32Lib.RVA = rData.getRVA();

            var symOffsets = Arraylist(u32).initCapacity(allocator, pe32Lib.symRVAs.len) catch unreachable;

            for (pe32Lib.symRVAs) |symRVA| {
                symOffsets.appendAssumeCapacity(@as(u32, rData.buffer.items.len));
                rData.buffer.appendSlice(std.mem.asBytes(&@as(u64, @intCast(symRVA)))) catch unreachable;
            }

            rDataOut.libOffsets.appendAssumeCapacity(LibOffsets{ .symOffsets = symOffsets.items });
            rData.buffer.appendSlice(std.mem.asBytes(&@as(u64, 0))) catch unreachable;
            rDataOut.libOffsets.appendAssumeCapacity(.{ .symOffsets = symOffsets.items });
        }

        const IATSize = @as(u32, rData.buffer.items.len);

        for (pe32Libs.items) |*peLib| {
            peLib.imgThunkRVA = rData.getRVA();

            for (peLib.symRVAs) |symRVA| {
                rData.buffer.appendSlice(std.mem.asBytes(&@as(u64, @intCast(symRVA)))) catch unreachable;
            }

            rData.buffer.appendSlice(std.mem.asBytes(&@as(u64, 0))) catch unreachable;
        }

        for (external.libNames, 0..) |libName, idx| {
            var lib = &pe32Libs.items[idx];
            lib.nameRVA = rData.getRVA();
            rData.appendStr(libName);
        }

        const importDirRVA = rData.getRVA();

        for (pe32Libs.items) |pe32Lib| {
            rData.buffer.appendSlice(std.mem.asBytes(&std.mem.zeroInit(ImageImportDescriptor, ImageImportDescriptor{
                .caracsOrOriginalFirstThunk = pe32Lib.imgThunkRVA,
                .name = pe32Lib.nameRVA,
                .firstThunk = pe32Lib.RVA,
            }))) catch unreachable;
        }

        const importDirSize = rData.getRVA() - importDirRVA;
        if (importDirSize > 0) rData.buffer.appendSlice(std.mem.asBytes(&std.mem.zeroes(ImageImportDescriptor))) catch unreachable;

        rData.header.misc.virtualSize = @as(u32, @intCast(rData.buffer.items.len));
        rData.header.rowDataSize = @as(u32, @intCast(std.mem.alignForward(rData.buffer.items.len, fileAlignment)));
        offset.afterSize(rData.header.rowDataSize);
        rDataOut.iat = .{
            .RVA = IATRVA,
            .size = IATSize,
        };

        rDataOut.import = .{
            .RVA = importDirRVA,
            .size = importDirSize,
        };
        return rData;
    }

    pub const Text = struct {
        pub const EncodingOutput = struct {
            patches: Arraylist(Patch),
        };
    };

    fn encodeTextSection(exec: anytype, allocator: *Allocator, textOut: *Text.EncodingOutput, arch: std.Target.Cpu.Arch, offset: *Offset) Section {
        var text = Section{
            .header = std.mem.zeroInit(ImageSectionHeader, ImageSectionHeader{
                .ptrToRawData = offset.file,
                .virtualAddress = offset.virtual,
                .name = Section.names[@intFromEnum(Section.Index.@".text")],
                .caracs = Section.caracteristics[@intFromEnum(Section.Index.@".text")],
            }),
            .buffer = Arraylist(u8).init(allocator.*),
        };

        switch (arch) {
            .x86_64 => {
                var prog = @as(*Program, @ptrCast(exec));
                prog.encodePeTextSection(allocator, text, textOut, offset);
            },
        }
        return text;
    }
};

const LibOffsets = struct {
    symOffsets: []u32,
};

pub const Offset = struct {
    file: u32,
    virtual: u32,
    const Self = @This();

    pub fn afterSize(self: *Self, size: u32) void {
        self.* = .{
            .file = self.file + @as(u32, @intCast(std.mem.alignForward(size, fileAlignment))),
            .virtual = self.virtual + @as(u32, @intCast(std.mem.alignForward(size, fileAlignment))),
        };
    }
};

const ImportedLib = struct {
    nameRVA: u32,
    RVA: u32,
    imgThunkRVA: u32,
    symRVAs: []u32,
};

pub const Patch = struct {
    sourceSectionBuffer: u32,
    destinationSectionBuffer: u32,
    section2ReadFrom: Section.Index,
    section2WriteTo: Section.Index,
};
