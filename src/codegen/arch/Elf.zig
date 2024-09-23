const std = @import("std");

const FileHeade = extern struct {
    magic: u8 = 0x7F,
    elfId: [3]u8 = "ELF".*,
    bitCount: u8 = @intFromEnum(Bits.B64),
    endianness: u8 = @intFromEnum(Endianness.little),
    headerVersion: u8 = 1,
    osAbi: u8 = @intFromEnum(ABI.SystemV),
    abiVersion: u8 = 0,
    padding: [7]u8 = [_]u8{0} ** 7,
    objectType: u16 = @intFromEnum(ObjectFileType.executable),
    machine: u16 = @intFromEnum(Machine.AMD64),
    version: u32 = 1,
    entryPoint: u64,
    programHeaderOffset: u64 = 0x40,
    sectionHeaderOffset: u64,
    flags: u32 = 0,
    headerSize: u16 = 0x40,
    programHeaderSize: u16 = @sizeOf(ProgramHeader),
    programHeaderEntryCount: u16 = 1,
    sectionHeaderSize: u16 = @sizeOf(SectionHeader),
    sectionHeaderEntryCount: u16,
    sectionHeaderStringIndex: u16,

    const Bits = enum(u8) {
        b32 = 1,
        b64 = 2,
    };

    const Endianness = enum(u8) {
        little = 1,
        big = 2,
    };

    const ABI = enum(u8) {
        SystemV = 0,
    };

    const ObjectFileType = enum(u16) {
        none = 0,
        relocatable = 1,
        executable = 2,
        shared = 3,
        core = 4,
        lowOs = 0xFE00,
        highOs = 0xFEFF,
        lowProcessor = 0xFF00,
        highProcessor = 0xFFFF,
    };

    const Machine = enum(u16) {
        AMD64 = 0x3E,
    };
};

const ProgramHeader = extern struct {
    type: u32 = @intFromEnum(ProgramHeaderType.load),
    flags: u32 = @intFromEnum(Flags.readable) | @intFromEnum(Flags.executable),
    offset: u64,
    virtualAddress: u64,
    physicalAddress: u64,
    fileSize: u64,
    memorySize: u64,
    alignment: u64 = 0x1000,

    const ProgramHeaderType = enum(u32) {
        null = 0,
        load = 1,
        dynamic = 2,
        interpreter = 3,
        note = 4,
        shlib = 5,
        phdr = 6,
        tls = 7,
        lowOs = 0x60000000,
        highOs = 0x6FFFFFFF,
        lowProcessor = 0x70000000,
        highProcessor = 0x7FFFFFFF,
    };

    const Flags = enum(u8) {
        executable = 1,
        writable = 2,
        readable = 4,
    };
};

const SectionHeader = extern struct {
    nameOffset: u32,
    type: u32,
    flags: u64,
    address: u64,
    offset: u64,
    size: u64,
    link: u32,
    info: u32,
    addressAlignment: u64,
    entrySize: u64,

    const ID = enum(u8) {
        null = 0,
        programData = 1,
        symbolTable = 2,
        stringTable = 3,
        relocationAddends = 4,
        symbolHashTable = 5,
        dynamicLinkingTable = 6,
        note = 7,
        programSpaceNoData = 8,
        relocationEntries = 9,
        reserved = 10,
        dynamicLinkingSymbolTable = 11,
        arrayOfConstructors = 14,
        arrayOfDestructors = 15,
        arrayOfPreConstructors = 16,
        sectionGroup = 17,
        extendedSectionIndices = 18,
        numberOfDefinedTypes = 19,
        startOsSpecific = 0x60000000,
    };

    const Flag = enum(u64) {
        writable = 0x01,
        allocatable = 0x02,
        executable = 0x04,
        mergeable = 0x10,
        containsNullTerminatedStrings = 0x20,
        infoLink = 0x40,
        linkOrder = 0x80,
        osNonConforming = 0x100,
        sectionGroup = 0x200,
        tls = 0x400,
        maskOsSpecific = 0x0FF00000,
        maskProcessorSpecific = 0xF0000000,
        ordered = 0x40000000,
        exclude = 0x80000000,
    };
};
