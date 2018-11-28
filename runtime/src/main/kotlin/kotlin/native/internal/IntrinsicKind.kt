package kotlin.native.internal

enum class IntrinsicKind {
    PLUS,
    MINUS,
    TIMES,
    DIV,
    REM,
    INC,
    DEC,
    UNARY_PLUS,
    UNARY_MINUS,
    SHL,
    SHR,
    USHR,
    AND,
    OR,
    XOR,
    INV,
    PRIMITIVE_CAST,
    COMPARE_TO
}