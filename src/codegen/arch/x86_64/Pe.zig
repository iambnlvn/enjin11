const std = @import("std");
const Allocator = std.mem.Allocator;
const Arraylist = std.ArrayList;
const panic = std.debug.panic;
const Semantics = @import("./../../../Sem.zig");
const Program = @import("Program.zig").Program;
const Parser = @import("./../../../Parser.zig");
const writeExeFile = @import("./../../gen.zig").writeExeFile;

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
pub const sectionAlignment = 0x1000;

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

const ImageDosHeader = extern struct {
    magNum: u16,
    bytesOnLastPage: u16,
    pagesInFile: u16,
    relocations: u16,
    headerSize: u16,
    minExtraParag: u16,
    maxExtraParag: u16,
    initialSS: u16,
    initialSP: u16,
    checksum: u16,
    initialIP: u16,
    initialCS: u16,
    relocTableOffset: u16,
    overlayNumber: u16,
    reserved: [4]u16,
    OEMID: u16,
    OEMInfo: u16,
    reserved2: [10]u16,
    newHeaderOffset: u32,
};

const ImageFileHeader = extern struct {
    machine: u16,
    sectionCount: u16,
    timeDateStamp: u32,
    ptrToSymbolTable: u32,
    symbolCount: u32,
    optionalHeaderSize: u16,
    caracs: u16,

    const caracs = enum(u16) {
        relocationsStripped = 0x0001,
        executable = 0x0002,
        lineNumsStripped = 0x0004,
        localSymsStripped = 0x0008,
        aggresiveWsTrim = 0x0010,
        largeAddressAware = 0x0020,
        bytesReservedLow = 0x0080,
        _32bitMachine = 0x0100,
        debugStripped = 0x0200,
        runFromSwap = 0x0400,
        netRunFromSwap = 0x0800,
        system = 0x1000,
        dll = 0x2000,
        upSystemOnly = 0x4000,
        bytesReservedHigh = 0x8000,
    };
};
const imageNumOfDir = 0x10;
const ImageOptionalHeader = extern struct {
    mag: u16,
    majorLinkerVersion: u8,
    minorLinkerVersion: u8,
    codeSize: u32,
    initializedDataSize: u32,
    uninitializedDataSize: u32,
    entryPointRVA: u32,
    baseOfCode: u32,
    imageBase: u64,
    sectionAlignment: u32,
    fileAlignment: u32,
    majorOSVersion: u16,
    minorOSVersion: u16,
    majorImageVersion: u16,
    minorImageVersion: u16,
    majorSubsystemVersion: u16,
    minorSubsystemVersion: u16,
    win32VersionValue: u32,
    sizeOfImage: u32,
    sizeOfHeaders: u32,
    checkSum: u32,
    subsystem: u16,
    DLLCaracs: u16,
    sizeOfStackReserve: u64,
    sizeOfStackCommit: u64,
    sizeOfHeapReserve: u64,
    sizeOfHeapCommit: u64,
    loaderFlags: u32,
    numberOfRVAandSizes: u32,
    dataDirectory: [imageNumOfDir]Section.Directory,

    const magic = 0x20B;
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

    fn encodeDataSection(allocator: *Allocator, dataBuffer: []const u8, offset: *Offset) Section {
        const dataBufferLen = if (dataBuffer.len == 0) 1 else dataBuffer.len;
        var data = Section{
            .header = std.mem.zeroInit(ImageSectionHeader, ImageSectionHeader{
                .ptrToRawData = offset.file,
                .virtualAddress = offset.virtual,
                .name = Section.names[@intFromEnum(Section.Index.@".data")],
                .caracs = Section.caracteristics[@intFromEnum(Section.Index.@".data")],
            }),
            .buffer = Arraylist(u8).initCapacity(allocator, dataBufferLen) catch unreachable,
        };

        if (dataBuffer.len == 0)
            data.buffer.appendAssumeCapacity(0)
        else
            data.buffer.appendSliceAssumeCapacity(dataBuffer);

        data.header.misc.virtualSize = @as(u32, @intCast(dataBufferLen));
        data.header.rowDataSize = @as(u32, @intCast(std.mem.alignForward(dataBufferLen, fileAlignment)));
        offset.afterSize(data.header.rowDataSize);

        return data;
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

const imageNTSignature: u32 = 0x00004550;
const imageDosSignature: u16 = 0x5A4D;

pub fn write(allocator: *Allocator, exec: anytype, dataBuffer: []const u8, execFileName: []const u8, external: Semantics.External, target: std.Target) void {
    const nullSectionHeaderCount = 1;
    var sectionHeaderSize = @sizeOf(ImageSectionHeader) * (Section.count + nullSectionHeaderCount);

    const fileSizeOfHeaders = @as(u32, @intCast(std.mem.alignForward(@sizeOf(ImageDosHeader) + @sizeOf(@TypeOf(imageNTSignature)) + @sizeOf(ImageFileHeader) + @sizeOf(ImageOptionalHeader) + sectionHeaderSize, fileAlignment)));

    var offset = Offset{ .file = @as(u32, std.mem.alignForward(fileSizeOfHeaders, fileAlignment)), .virtual = @as(u32, @intCast(std.mem.alignForward(fileSizeOfHeaders, sectionAlignment))) };
    var outText: Section.Text.EncodingOutput = undefined;
    var rDataOut: Section.Rdata.EncodingOutput = undefined;

    var sections = [Section.count]Section{
        Section.encodeTextSection(exec, allocator, &outText, target.cpu.arch, &offset),
        Section.encodeRdataSection(allocator, external, &rDataOut, &offset),
        Section.encodeDataSection(allocator, dataBuffer, &offset),
    };

    for (outText.patches) |patch| {
        const section2Patch = &sections[@intFromEnum(patch.section2WriteTo)];

        const jmpFromRVA = section2Patch.header.virtualAddress + patch.sourceSectionBuffer + 4;
        var patchAddr = @as(*align(1) i32, @ptrCast(&section2Patch.buffer.items[patch.destinationSectionBuffer]));

        const jmp2RVA = switch (patch.section2ReadFrom) {
            .@".rdata" => blk: {
                const idx = Parser.Function.External.Index.fromU32(patch.sourceSectionBuffer);
                const symOffset = rDataOut.libOffsets.items[idx.library].symOffsets[idx.function];
                break :blk sections[@intFromEnum(Section.Index.@".rdata")].header.virtualAddress + symOffset;
            },
            .@".data" => blk: {
                const symOffset = patch.sourceSectionBuffer;
                break :blk sections[@intFromEnum(Section.Index.@".data")].header.virtualAddress + symOffset;
            },
            else => panic("Invalid section to patch"),
        };
        patchAddr.* = @as(i32, @intCast(@as(i64, @intCast(jmp2RVA)) - @as(i64, @intCast(jmpFromRVA))));
    }

    const virtualSize = offset.virtual;
    const maxExecBufferSize = offset.file;

    var exeBuffer = Arraylist(u8).initCapacity(allocator, maxExecBufferSize) catch unreachable;

    exeBuffer.appendSlice(std.mem.asBytes(&std.mem.zeroInit(ImageDosHeader, .{
        .magNum = imageDosSignature,
        .newHeaderOffset = @sizeOf(ImageDosHeader),
    }))) catch unreachable;

    exeBuffer.appendSlice(std.mem.asBytes(&imageNTSignature)) catch unreachable;
    exeBuffer.appendSlice(std.mem.asBytes(std.mem.zeroInit(ImageFileHeader, ImageFileHeader{
        .machine = @intFromEnum(switch (target.cpu.arch) {
            .x86_64 => ImageFileMachine.amd64,
            .aarch64 => ImageFileMachine.arm64,
            else => panic("Unsupported Cpu Architecture\n", .{target.cpu.arch}),
        }),
        .sectionCount = Section.count,
        .timeDateStamp = @as(u32, @intCast(@as(i32, @intCast(std.time.timestamp())))),
        .optionalHeaderSize = @sizeOf(ImageOptionalHeader),
        .caracs = @intFromEnum(ImageFileHeader.caracs.executable) | @intFromEnum(ImageFileHeader.caracs.largeAddressAware),
    }))) catch unreachable;

    const imageOptionalHeaderIdx = exeBuffer.items.len;
    exeBuffer.appendSlice(std.mem.asBytes(&std.mem.zeroInit(ImageOptionalHeader, ImageOptionalHeader{
        .mag = ImageOptionalHeader.magic,
        .majorLinkerVersion = 1,
        .minorLinkerVersion = 0,
        .codeSize = sections[@intFromEnum(Section.Index.@".text")].header.rowDataSize,
        .initializedDataSize = sections[@intFromEnum(Section.Index.@".rdata")].header.rowDataSize + sections[@intFromEnum(Section.Index.@".data")].header.rowDataSize,
        .entryPointRVA = sections[@intFromEnum(Section.Index.@".rdata")].header.rowDataSize + sections[@intFromEnum(Section.Index.@".data")].header.rowDataSize,
        .baseOfCode = sections[@intFromEnum(Section.Index.@".text")].header.virtualAddress,
        .imageBase = 0x0000000140000000,
        .sectionAlignment = sectionAlignment,
        .fileAlignment = fileAlignment,
        .majorOSVersion = 6,
        .minorOSVersion = 0,
        .majorImageVersion = 0,
        .majorSubsystemVersion = 6,
        .minorSubsystemVersion = 0,
        .subsystem = 3,
        .sizeOfHeaders = fileSizeOfHeaders,
        .sizeOfImage = virtualSize,
        .checkSum = 0,
        .DLLCaracs = @intFromEnum(ImageDLLCharacteristics.highEntropyVA) | @intFromEnum(ImageDLLCharacteristics.nx_compat) | @intFromEnum(ImageDLLCharacteristics.dynamicBase) | @intFromEnum(ImageDLLCharacteristics.terminalServerAware),
        .sizeOfStackReserve = 0x100000,
        .sizeOfStackCommit = 0x1000,
        .sizeOfHeapReserve = 0x100000,
        .sizeOfHeapCommit = 0x1000,
        .numberOfRVAandSizes = imageNumOfDir,
    }))) catch panic("Failed to append Optional headers\n", .{});

    var imageOptionalHeader = @as(*align(1) ImageOptionalHeader, @ptrFromInt(@intFromPtr(exeBuffer.items.ptr) + imageOptionalHeaderIdx));

    if (rDataOut.iat.size > 0) {
        var importDataDir = &imageOptionalHeader.dataDirectory[@intFromEnum(DirectoryIdx.import)];
        importDataDir.* = rDataOut.import;
    }

    for (sections) |*section| {
        exeBuffer.appendSlice(std.mem.asBytes(&section.header)) catch unreachable;
    }

    exeBuffer.appendSlice(std.mem.asBytes(&std.mem.zeroes(ImageSectionHeader))) catch unreachable;

    for (sections) |*section| {
        exeBuffer.items.len = section.header.ptrToRawData;
        exeBuffer.appendSlice(section.buffer.items) catch unreachable;
    }

    exeBuffer.items.len = offset.file;
    writeExeFile(execFileName, exeBuffer.items);
}

const ImageFileMachine = enum(u16) {
    unknown = 0,
    tarHost = 0x0001,
    amd64 = 0x8664,
    arm64 = 0xAA64,

    const axp64 = ImageFileMachine.alpha64;
};

const ImageDLLCharacteristics = enum(u16) {
    highEntropyVA = 0x0020,
    dynamicBase = 0x0040,
    forceIntegrity = 0x0080,
    nxCompat = 0x0100,
    noIsolation = 0x0200,
    noSeh = 0x0400,
    noBind = 0x0800,
    appContainer = 0x1000,
    wdmDriver = 0x2000,
    guardCf = 0x4000,
    terminalServerAware = 0x8000,
};
