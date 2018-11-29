package kotlin.native.internal

class IntrinsicKind {
    companion object {
        const val PLUS              = "PLUS"
        const val MINUS             = "MINUS"
        const val TIMES             = "TIMES"
        const val DIV               = "DIV"
        const val REM               = "REM"
        const val INC               = "INC"
        const val DEC               = "DEC"
        const val UNARY_PLUS        = "UNARY_PLUS"
        const val UNARY_MINUS       = "UNARY_MINUS"
        const val SHL               = "SHL"
        const val SHR               = "SHR"
        const val USHR              = "USHR"
        const val AND               = "AND"
        const val OR                = "OR"
        const val XOR               = "XOR"
        const val INV               = "INV"
        const val PRIMITIVE_CAST    = "PRIMITIVE_CAST"
        const val COMPARE_TO        = "COMPARE_TO"
        const val NOT               = "NOT"
        const val TO_BITS           = "TO_BITS"
        const val FROM_BITS         = "FROM_BITS"
    }
}