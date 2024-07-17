pub const ID = struct {
    pub const Builtin = enum {
        os,
    };
    pub const Global = enum {
        Modules,
        ResolvedInternalFn,
        ResolvedExternalFn,
        ResolvedInvokeExpr,
    };
    pub const Scope = enum {
        IntegerLiterals,
        StructLiterals,
        ArrayLiterals,
        IdentifierExpr,
        InvokeExpr,
        FieldAccessExpr,
        ReturnExpr,
        BreakExpr,
        ArraySubscriptExpr,
        ArithmeticExpr,
        VarDeclaration,
        Assignment,
        CompoundAssignment,
        Comparison,
        Loops,
        Branches,
        Args,
        Field,
    };
    pub const Module = enum {
        InternalFn,
        ExternalFn,
        Imported,
    };
    pub const Level = enum(u2) {
        builtin,
        module,
        global,
        scope,
        pub const position = @bitSizeOf(u64) - @bitSizeOf(Level);
    };
};
