package org.jetbrains.kotlin.backend.konan.llvm

import kotlinx.cinterop.toCValues
import llvm.*
import org.jetbrains.kotlin.backend.konan.descriptors.coolInstrinsicAnnotation
import org.jetbrains.kotlin.backend.konan.reportCompilationError
import org.jetbrains.kotlin.ir.declarations.IrFunction
import org.jetbrains.kotlin.ir.expressions.IrCall
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.name.Name

// TODO: Comment each entry
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
    COMPARE_TO,
    NOT,
    TO_BITS,
    FROM_BITS
}

internal class IntrinsicGenerator(val codegen: CodeGenerator) {

    private val context = codegen.context

    private val integralTypesOrder = arrayOf(int1Type, int8Type, int16Type, int32Type, int64Type)
    private val realTypesOrder = arrayOf(floatType, doubleType)
    
    private val IrFunction.llvmReturnType: LLVMTypeRef
        get() {
            val llvmFunctionTy = codegen.getLlvmFunctionType(this)
            return LLVMGetReturnType(llvmFunctionTy)!!
        }

    // TODO: it can be better
    private fun getIntrinsicKind(function: IrFunction): IntrinsicKind {
        val annotation = function.descriptor.annotations.findAnnotation(coolInstrinsicAnnotation)!!
        val value = annotation.allValueArguments[Name.identifier("kind")]!!.value as String
        return IntrinsicKind.valueOf(value)
    }

    // TODO: set debug info
    fun evaluateCall(callee: IrCall, args: List<LLVMValueRef>, functionGenerationContext: FunctionGenerationContext): LLVMValueRef =
        when (getIntrinsicKind(callee.symbol.owner)) {
            IntrinsicKind.PLUS ->           ::emitPlus
            IntrinsicKind.MINUS ->          ::emitMinus
            IntrinsicKind.TIMES ->          ::emitTimes
            IntrinsicKind.DIV ->            ::emitDiv
            IntrinsicKind.REM ->            ::emitRem
            IntrinsicKind.INC ->            ::emitInc
            IntrinsicKind.DEC ->            ::emitDec
            IntrinsicKind.UNARY_PLUS ->     ::emitUnaryPlus
            IntrinsicKind.UNARY_MINUS ->    ::emitUnaryMinus
            IntrinsicKind.SHL ->            ::emitShl
            IntrinsicKind.SHR ->            ::emitShr
            IntrinsicKind.USHR ->           ::emitUshr
            IntrinsicKind.AND ->            ::emitAnd
            IntrinsicKind.OR ->             ::emitOr
            IntrinsicKind.XOR ->            ::emitXor
            IntrinsicKind.INV ->            ::emitInv
            IntrinsicKind.COMPARE_TO ->     ::emitCompareTo
            IntrinsicKind.PRIMITIVE_CAST -> ::emitPrimitiveCast
            IntrinsicKind.NOT ->            ::emitNot
            IntrinsicKind.FROM_BITS ->      ::emitFromBits
            IntrinsicKind.TO_BITS ->        ::emitToBits
        } (callee, args, functionGenerationContext)!!

    private fun castArgs(builder: LLVMBuilderRef, args: List<LLVMValueRef>, destTy: LLVMTypeRef? = null): Pair<LLVMValueRef, LLVMValueRef> {
        val first = args[0]
        val second = args[1]
        val unifiedType = destTy ?: findUnifiedType(first.type, second.type)
        val firstResult = cast(builder, first, unifiedType)
        val secondResult = cast(builder, second, unifiedType)
        return firstResult to secondResult
    }

    // TODO: Unify with emitToBits?
    private fun emitFromBits(callee: IrCall, args: List<LLVMValueRef>, functionGenerationContext: FunctionGenerationContext) =
        with(functionGenerationContext) {
            LLVMBuildBitCast(builder, args[0], callee.symbol.owner.llvmReturnType, "")
        }

    private fun emitToBits(callee: IrCall, args: List<LLVMValueRef>, functionGenerationContext: FunctionGenerationContext) =
        with(functionGenerationContext) {
            LLVMBuildBitCast(builder, args[0], callee.symbol.owner.llvmReturnType, "")
        }

    private fun emitNot(callee: IrCall, args: List<LLVMValueRef>, functionGenerationContext: FunctionGenerationContext) =
        with(functionGenerationContext) {
            LLVMBuildNot(builder, args[0], "")
        }
    
    private fun emitPlus(callee: IrCall, args: List<LLVMValueRef>, functionGenerationContext: FunctionGenerationContext) =
        with(functionGenerationContext) {
            val function = callee.symbol.owner
            val (first, second) = castArgs(builder, args, function.llvmReturnType)
            if (first.type.isFloatingPoint()) {
                LLVMBuildFAdd(builder, first, second, "")
            } else {
                LLVMBuildAdd(builder, first, second, "")
            }
        }

    private fun emitPrimitiveCast(callee: IrCall, args: List<LLVMValueRef>, functionGenerationContext: FunctionGenerationContext) =
        with(functionGenerationContext) {
            val value = args[0]
            val destTy = callee.symbol.owner.llvmReturnType
            cast(builder, value, destTy)
        }

    private fun emitShl(callee: IrCall, args: List<LLVMValueRef>, functionGenerationContext: FunctionGenerationContext) =
        with(functionGenerationContext) {
            val first = args[0]
            val second = args[1]
            val shift = if (first.type == int64Type) {
                val tmp = LLVMBuildAnd(builder, second, Int32(63).llvm, "")
                LLVMBuildZExt(builder, tmp, int64Type, "")
            } else {
                LLVMBuildAnd(builder, second, Int32(31).llvm, "")
            }
            LLVMBuildShl(builder, first, shift, "")
        }

    private fun emitShr(callee: IrCall, args: List<LLVMValueRef>, functionGenerationContext: FunctionGenerationContext) =
        with(functionGenerationContext) {
            val first = args[0]
            val second = args[1]
            val shift = if (first.type == int64Type) {
                val tmp = LLVMBuildAnd(builder, second, Int32(63).llvm, "")
                LLVMBuildZExt(builder, tmp, int64Type, "")
            } else {
                LLVMBuildAnd(builder, second, Int32(31).llvm, "")
            }
            LLVMBuildAShr(builder, first, shift, "")
        }

    private fun emitUshr(callee: IrCall, args: List<LLVMValueRef>, functionGenerationContext: FunctionGenerationContext) =
        with(functionGenerationContext) {
            val first = args[0]
            val second = args[1]
            val shift = if (first.type == int64Type) {
                val tmp = LLVMBuildAnd(builder, second, Int32(63).llvm, "")
                LLVMBuildZExt(builder, tmp, int64Type, "")
            } else {
                LLVMBuildAnd(builder, second, Int32(31).llvm, "")
            }
            LLVMBuildLShr(builder, first, shift, "")
        }

    private fun emitAnd(callee: IrCall, args: List<LLVMValueRef>, functionGenerationContext: FunctionGenerationContext) =
        with(functionGenerationContext) {
            val function = callee.symbol.owner
            val (first, second) = castArgs(builder, args, function.llvmReturnType)
            LLVMBuildAnd(builder, first, second, "")
        }

    private fun emitOr(callee: IrCall, args: List<LLVMValueRef>, functionGenerationContext: FunctionGenerationContext) =
        with(functionGenerationContext) {
            val function = callee.symbol.owner
            val (first, second) = castArgs(builder, args, function.llvmReturnType)
            LLVMBuildOr(builder, first, second, "")
        }

    private fun emitXor(callee: IrCall, args: List<LLVMValueRef>, functionGenerationContext: FunctionGenerationContext) =
        with(functionGenerationContext) {
            val function = callee.symbol.owner
            val (first, second) = castArgs(builder, args, function.llvmReturnType)
            LLVMBuildXor(builder, first, second, "")
        }

    private fun emitInv(callee: IrCall, args: List<LLVMValueRef>, functionGenerationContext: FunctionGenerationContext) =
        with(functionGenerationContext) {
            val first = args[0]
            val mask = makeConstOfType(first.type, -1)
            LLVMBuildXor(builder, first, mask, "")
        }

    private fun emitMinus(callee: IrCall, args: List<LLVMValueRef>, functionGenerationContext: FunctionGenerationContext) =
        with (functionGenerationContext) {
            val function = callee.symbol.owner
            val (first, second) = castArgs(builder, args, function.llvmReturnType)
            if (first.type.isFloatingPoint()) {
                LLVMBuildFSub(builder, first, second, "")
            } else {
                LLVMBuildSub(builder, first, second, "")
            }    
        }

    private fun emitTimes(callee: IrCall, args: List<LLVMValueRef>, functionGenerationContext: FunctionGenerationContext) =
        with (functionGenerationContext) {
            val function = callee.symbol.owner
            val (first, second) = castArgs(builder, args, function.llvmReturnType)
            if (first.type.isFloatingPoint()) {
                LLVMBuildFMul(builder, first, second, "")
            } else {
                LLVMBuildMul(builder, first, second, "")
            }
        }

    private fun emitDiv(callee: IrCall, args: List<LLVMValueRef>, functionGenerationContext: FunctionGenerationContext) =
        with (functionGenerationContext) {
            val function = callee.symbol.owner
            val divider = args[1]
            if (divider.type.isFloatingPoint()) {
                return emitDivNoThrow(callee, args, functionGenerationContext)
            }
            // TODO: debug info
            val validArgBb = basicBlock(locationInfo = null)
            val invalidArgBb = basicBlock(locationInfo = null)

            val isZero = LLVMBuildICmp(builder, LLVMIntPredicate.LLVMIntEQ, divider, Zero(divider.type).llvm, "")!!
            condBr(isZero, invalidArgBb, validArgBb)

            LLVMPositionBuilderAtEnd(builder, invalidArgBb)
            val throwArthExc = codegen.llvmFunction(context.ir.symbols.throwArithmeticException.owner)
            val nullObj = listOf(codegen.kNullObjHeaderPtrPtr) // TODO: is it correct?
            LLVMBuildCall(builder, throwArthExc, nullObj.toCValues(), nullObj.size, "")
            LLVMBuildUnreachable(builder)

            LLVMPositionBuilderAtEnd(builder, validArgBb)
            val (first, second) = castArgs(builder, args, function.llvmReturnType)
            if (first.type.isFloatingPoint()) {
                LLVMBuildFDiv(builder, first, second, "")
            } else {
                LLVMBuildSDiv(builder, first, second, "")
            }
        }

    private fun emitDivNoThrow(callee: IrCall, args: List<LLVMValueRef>, functionGenerationContext: FunctionGenerationContext) =
        with (functionGenerationContext) {
            val function = callee.symbol.owner
            val (first, second) = castArgs(builder, args, function.llvmReturnType)
            if (first.type.isFloatingPoint()) {
                LLVMBuildFDiv(builder, first, second, "")
            } else {
                LLVMBuildSDiv(builder, first, second, "")
            }
        }

    private fun emitRem(callee: IrCall, args: List<LLVMValueRef>, functionGenerationContext: FunctionGenerationContext) =
        with (functionGenerationContext) {
            val function = callee.symbol.owner
            val (first, second) = castArgs(builder, args, function.llvmReturnType)
            if (first.type.isFloatingPoint()) {
                LLVMBuildFRem(builder, first, second, "")
            } else {
                LLVMBuildSRem(builder, first, second, "")
            }
        }

    private fun emitInc(callee: IrCall, args: List<LLVMValueRef>, functionGenerationContext: FunctionGenerationContext) =
        with (functionGenerationContext) {
            val first = args[0]
            val const1 = makeConstOfType(first.type, 1)
            if (first.type.isFloatingPoint()) {
                LLVMBuildFAdd(builder, first, const1, "")
            } else {
                LLVMBuildAdd(builder, first, const1, "")
            }
        }

    private fun emitDec(callee: IrCall, args: List<LLVMValueRef>, functionGenerationContext: FunctionGenerationContext) =
        with (functionGenerationContext) {
            val first = args[0]
            val const1 = makeConstOfType(first.type, 1)
            if (first.type.isFloatingPoint()) {
                LLVMBuildFSub(builder, first, const1, "")
            } else {
                LLVMBuildSub(builder, first, const1, "")
            }
        }

    private fun emitUnaryPlus(callee: IrCall, args: List<LLVMValueRef>, functionGenerationContext: FunctionGenerationContext) =
        with (functionGenerationContext) {
            val value = args[0]
            val destTy = callee.symbol.owner.llvmReturnType
            cast(builder, value, destTy)
        }

    private fun emitUnaryMinus(callee: IrCall, args: List<LLVMValueRef>, functionGenerationContext: FunctionGenerationContext) =
        with (functionGenerationContext) {
            val value = args[0]
            val destTy = callee.symbol.owner.llvmReturnType
            val first = cast(builder, value, destTy)
            val const0 = makeConstOfType(first.type, 0)
            if (first.type.isFloatingPoint()) {
                LLVMBuildFSub(builder, const0, first, "")
            } else {
                LLVMBuildSub(builder, const0, first, "")
            }
        }

    private fun emitCompareTo(callee: IrCall, args: List<LLVMValueRef>, functionGenerationContext: FunctionGenerationContext) =
        with (functionGenerationContext) {
            val (first, second) = castArgs(builder, args)
            val equal: LLVMValueRef
            val less: LLVMValueRef
            if (first.type.isFloatingPoint()) {
                equal = LLVMBuildFCmp(builder, LLVMRealPredicate.LLVMRealOEQ, first, second, "")!!
                less = LLVMBuildFCmp(builder, LLVMRealPredicate.LLVMRealOLT, first, second, "")!!
            } else {
                equal = LLVMBuildICmp(builder, LLVMIntPredicate.LLVMIntEQ, first, second, "")!!
                less = LLVMBuildICmp(builder, LLVMIntPredicate.LLVMIntSLT, first, second, "")!!
            }
            val tt = LLVMBuildSelect(builder, less, Int32(-1).llvm, Int32(1).llvm, "")
            LLVMBuildSelect(builder, equal, Int32(0).llvm, tt, "")
        }

    private fun makeConstOfType(type: LLVMTypeRef, value: Int): LLVMValueRef = when (type) {
        int8Type -> Int8(value.toByte()).llvm
        int16Type -> Char16(value.toChar()).llvm
        int32Type -> Int32(value).llvm
        int64Type -> Int64(value.toLong()).llvm
        floatType -> Float32(value.toFloat()).llvm
        doubleType -> Float64(value.toDouble()).llvm
        else -> context.reportCompilationError("Unexpected primitive type: $type")
    }

    // Assuming that both types are i*
    private fun compareIntegralTypes(firstTy: LLVMTypeRef, secondTy: LLVMTypeRef): Int {
        return integralTypesOrder.indexOf(firstTy).compareTo(integralTypesOrder.indexOf(secondTy))
    }

    private fun cast(builder: LLVMBuilderRef, value: LLVMValueRef, destTy: LLVMTypeRef): LLVMValueRef {
        if (value.type == destTy) return value

        if (value.type == floatType && destTy == doubleType) {
            return LLVMBuildFPExt(builder, value, destTy, "")!!
        }
        if (value.type == doubleType && destTy == floatType) {
            return LLVMBuildFPTrunc(builder, value, destTy, "")!!
        }
        if (destTy.isFloatingPoint() && !value.type.isFloatingPoint()) {
            return LLVMBuildSIToFP(builder, value, destTy, "")!!
        }
        val compResult = compareIntegralTypes(value.type, destTy)
        return when {
            compResult < 0 -> LLVMBuildSExt(builder, value, destTy, "")!!
            compResult > 0 -> LLVMBuildTrunc(builder, value, destTy, "")!!
            else -> value
        }
    }

    private fun findUnifiedType(firstTy: LLVMTypeRef, secondTy: LLVMTypeRef): LLVMTypeRef {
        // NB: Order of `if` matters!
        if (firstTy == doubleType || secondTy == doubleType) {
            return doubleType
        }
        if (firstTy == floatType || secondTy == floatType) {
            return floatType
        }
        if (firstTy == int64Type || secondTy == int64Type) {
            return int64Type
        }
        if (firstTy == int32Type || secondTy == int32Type) {
            return int32Type
        }
        if (firstTy == int16Type || secondTy == int16Type) {
            return int16Type
        }
        if (firstTy == int8Type || secondTy == int8Type) {
            return int8Type
        }
        if (firstTy == int1Type || secondTy == int1Type) {
            return int1Type
        }
        error("Unexpected types: $firstTy $secondTy")
    }
}