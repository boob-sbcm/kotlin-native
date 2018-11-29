package org.jetbrains.kotlin.backend.konan.llvm

import kotlinx.cinterop.toCValues
import llvm.*
import org.jetbrains.kotlin.backend.konan.reportCompilationError
import org.jetbrains.kotlin.ir.declarations.IrFunction
import org.jetbrains.kotlin.ir.util.hasAnnotation
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.name.Name

internal val coolInstrinsicFqName = FqName("kotlin.native.internal.CoolIntrinsic")

// TODO: Reuse IntrinsicKind in runtime and compiler
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

internal class IntrinsicGenerator(val codegen: CodeGenerator) {

    val context = codegen.context

    private val integralTypesOrder = arrayOf(int1Type, int8Type, int16Type, int32Type, int64Type)
    private val realTypesOrder = arrayOf(floatType, doubleType)

    private fun getIntrinsicKind(function: IrFunction): IntrinsicKind {
        val annotation = function.descriptor.annotations.findAnnotation(coolInstrinsicFqName)!!
        val value = annotation.allValueArguments[Name.identifier("kind")]!!.value as Pair<*, Name>
        return IntrinsicKind.valueOf(value.second.asString())
    }

    fun tryGenerate(function: IrFunction): Boolean =
        if (!function.hasAnnotation(coolInstrinsicFqName)) {
            false
        } else {
            when (getIntrinsicKind(function)) {
                IntrinsicKind.PLUS -> emitPlus(function)
                IntrinsicKind.MINUS -> emitMinus(function)
                IntrinsicKind.TIMES -> emitTimes(function)
                IntrinsicKind.DIV -> emitDiv(function)
                IntrinsicKind.REM -> emitRem(function)
                IntrinsicKind.INC -> emitInc(function)
                IntrinsicKind.DEC -> emitDec(function)
                IntrinsicKind.UNARY_PLUS -> emitUnaryPlus(function)
                IntrinsicKind.UNARY_MINUS -> emitUnaryMinus(function)
                IntrinsicKind.SHL -> emitShl(function)
                IntrinsicKind.SHR -> emitShr(function)
                IntrinsicKind.USHR -> emitUshr(function)
                IntrinsicKind.AND -> emitAnd(function)
                IntrinsicKind.OR -> emitOr(function)
                IntrinsicKind.XOR -> emitXor(function)
                IntrinsicKind.INV -> emitInv(function)
                IntrinsicKind.COMPARE_TO -> emitCompareTo(function)
                IntrinsicKind.PRIMITIVE_CAST -> emitPrimitiveCast(function)
            }
            true
        }

    private fun emitPrimitiveCast(function: IrFunction) {
        val builder = binopPrologue(function)
        val value = codegen.param(function, 0)
        val llvmFunctionTy = getFunctionType(codegen.llvmFunction(function))
        val destTy = LLVMGetReturnType(llvmFunctionTy)!!
        val result = cast(builder, value, destTy)
        LLVMBuildRet(builder, result)
    }

    private fun emitShl(function: IrFunction) {
        val builder = binopPrologue(function)

        val first = codegen.param(function, 0)
        val second = codegen.param(function, 1)

        val shift = if (first.type == int64Type) {
            val tmp = LLVMBuildAnd(builder, second, Int32(63).llvm, "")
            LLVMBuildZExt(builder, tmp, int64Type, "")
        } else {
            LLVMBuildAnd(builder, second, Int32(31).llvm, "")
        }
        val result = LLVMBuildShl(builder, first, shift, "")
        LLVMBuildRet(builder, result)
    }

    private fun emitShr(function: IrFunction) {
        val builder = binopPrologue(function)

        val first = codegen.param(function, 0)
        val second = codegen.param(function, 1)

        val shift = if (first.type == int64Type) {
            val tmp = LLVMBuildAnd(builder, second, Int32(63).llvm, "")
            LLVMBuildZExt(builder, tmp, int64Type, "")
        } else {
            LLVMBuildAnd(builder, second, Int32(31).llvm, "")
        }
        val result = LLVMBuildAShr(builder, first, shift, "")
        LLVMBuildRet(builder, result)
    }

    private fun emitUshr(function: IrFunction) {
        val builder = binopPrologue(function)

        val first = codegen.param(function, 0)
        val second = codegen.param(function, 1)

        val shift = if (first.type == int64Type) {
            val tmp = LLVMBuildAnd(builder, second, Int32(63).llvm, "")
            LLVMBuildZExt(builder, tmp, int64Type, "")
        } else {
            LLVMBuildAnd(builder, second, Int32(31).llvm, "")
        }
        val result = LLVMBuildLShr(builder, first, shift, "")
        LLVMBuildRet(builder, result)
    }

    private fun emitAnd(function: IrFunction) {
        val builder = binopPrologue(function)
        val (first, second) = castParameters(builder, function, useReturnType=true)
        val result = LLVMBuildAnd(builder, first, second, "")
        LLVMBuildRet(builder, result)
    }

    private fun emitOr(function: IrFunction) {
        val builder = binopPrologue(function)
        val (first, second) = castParameters(builder, function, useReturnType=true)
        val result = LLVMBuildOr(builder, first, second, "")
        LLVMBuildRet(builder, result)
    }

    private fun emitXor(function: IrFunction) {
        val builder = binopPrologue(function)
        val (first, second) = castParameters(builder, function, useReturnType=true)
        val result = LLVMBuildXor(builder, first, second, "")
        LLVMBuildRet(builder, result)
    }

    private fun emitInv(function: IrFunction) {
        val builder = binopPrologue(function)
        val first = codegen.param(function, 0)
        val mask = makeConstOfType(first.type, -1)
        val result = LLVMBuildXor(builder, first, mask, "")
        LLVMBuildRet(builder, result)
    }

    //TODO: Add always_inline, debug info.
    private fun emitPlus(function: IrFunction) {
        val builder = binopPrologue(function)
        val (first, second) = castParameters(builder, function, useReturnType=true)
        val result = if (first.type.isFloatingPoint()) {
            LLVMBuildFAdd(builder, first, second, "")
        } else {
            LLVMBuildAdd(builder, first, second, "")
        }
        LLVMBuildRet(builder, result)
    }

    private fun emitMinus(function: IrFunction) {
        val builder = binopPrologue(function)
        val (first, second) = castParameters(builder, function, useReturnType=true)
        val result = if (first.type.isFloatingPoint()) {
            LLVMBuildFSub(builder, first, second, "")
        } else {
            LLVMBuildSub(builder, first, second, "")
        }
        LLVMBuildRet(builder, result)
    }

    private fun emitTimes(function: IrFunction) {
        val builder = binopPrologue(function)
        val (first, second) = castParameters(builder, function, useReturnType=true)
        val result = if (first.type.isFloatingPoint()) {
            LLVMBuildFMul(builder, first, second, "")
        } else {
            LLVMBuildMul(builder, first, second, "")
        }
        LLVMBuildRet(builder, result)
    }

    private fun emitDiv(function: IrFunction) {
        val sp = codegen.param(function, 1)
        if (sp.type.isFloatingPoint()) {
            return emitDivNoThrow(function)
        }
        val llvmFunction = codegen.llvmFunction(function)
        val builder = binopPrologue(function)
        val validArgBb = LLVMAppendBasicBlock(llvmFunction, "")!!
        val invalidArgBb = LLVMAppendBasicBlock(llvmFunction, "")!!

        val isZero = LLVMBuildICmp(builder, LLVMIntPredicate.LLVMIntEQ, sp, Zero(sp.type).llvm, "")!!
        LLVMBuildCondBr(builder, isZero, invalidArgBb, validArgBb)

        LLVMPositionBuilderAtEnd(builder, validArgBb)
        val (first, second) = castParameters(builder, function, useReturnType=true)
        val result = if (first.type.isFloatingPoint()) {
            LLVMBuildFDiv(builder, first, second, "")
        } else {
            LLVMBuildSDiv(builder, first, second, "")
        }
        LLVMBuildRet(builder, result)

        LLVMPositionBuilderAtEnd(builder, invalidArgBb)
        val throwArthExc = codegen.llvmFunction(context.ir.symbols.throwArithmeticException.owner)
        val args = listOf(codegen.kNullObjHeaderPtrPtr) // TODO: is it correct?
        LLVMBuildCall(builder, throwArthExc, args.toCValues(), args.size, "")
        LLVMBuildUnreachable(builder)

    }

    private fun emitDivNoThrow(function: IrFunction) {
        val builder = binopPrologue(function)
        val (first, second) = castParameters(builder, function, useReturnType=true)
        val result = if (first.type.isFloatingPoint()) {
            LLVMBuildFDiv(builder, first, second, "")
        } else {
            LLVMBuildSDiv(builder, first, second, "")
        }
        LLVMBuildRet(builder, result)
    }

    private fun emitRem(function: IrFunction) {
        val builder = binopPrologue(function)
        val (first, second) = castParameters(builder, function, useReturnType=true)
        val result = if (first.type.isFloatingPoint()) {
            LLVMBuildFRem(builder, first, second, "")
        } else {
            LLVMBuildSRem(builder, first, second, "")
        }
        LLVMBuildRet(builder, result)
    }

    private fun emitInc(function: IrFunction) {
        val builder = binopPrologue(function)
        val first = codegen.param(function, 0)
        val const1 = makeConstOfType(first.type, 1)
        val result = if (first.type.isFloatingPoint()) {
            LLVMBuildFAdd(builder, first, const1, "")
        } else {
            LLVMBuildAdd(builder, first, const1, "")
        }
        LLVMBuildRet(builder, result)
    }

    private fun emitDec(function: IrFunction) {
        val builder = binopPrologue(function)
        val first = codegen.param(function, 0)
        val const1 = makeConstOfType(first.type, 1)
        val result = if (first.type.isFloatingPoint()) {
            LLVMBuildFSub(builder, first, const1, "")
        } else {
            LLVMBuildSub(builder, first, const1, "")
        }
        LLVMBuildRet(builder, result)
    }

    private fun emitUnaryPlus(function: IrFunction) {
        val builder = binopPrologue(function)
        val value = codegen.param(function, 0)
        val llvmFunctionTy = getFunctionType(codegen.llvmFunction(function))
        val destTy = LLVMGetReturnType(llvmFunctionTy)!!
        val result = cast(builder, value, destTy)
        LLVMBuildRet(builder, result)
    }

    private fun emitUnaryMinus(function: IrFunction) {
        val builder = binopPrologue(function)
        val value = codegen.param(function, 0)
        val llvmFunctionTy = getFunctionType(codegen.llvmFunction(function))
        val destTy = LLVMGetReturnType(llvmFunctionTy)!!
        val first = cast(builder, value, destTy)
        val const0 = makeConstOfType(first.type, 0)
        val result = if (first.type.isFloatingPoint()) {
            LLVMBuildFSub(builder, first, const0, "")
        } else {
            LLVMBuildSub(builder, first, const0, "")
        }
        LLVMBuildRet(builder, result)
    }

    private fun emitCompareTo(function: IrFunction) {
        val builder = binopPrologue(function)
        val (first, second) = castParameters(builder, function, useReturnType=false)
        val equal: LLVMValueRef
        val less: LLVMValueRef
        if (first.type.isFloatingPoint()) {
            equal = LLVMBuildFCmp(builder, LLVMRealPredicate.LLVMRealOEQ, first, second, "")!!
            less = LLVMBuildFCmp(builder, LLVMRealPredicate.LLVMRealOLT, first, second, "")!!
        } else {
            equal = LLVMBuildICmp(builder, LLVMIntPredicate.LLVMIntEQ, first, second, "")!!
            less = LLVMBuildICmp(builder, LLVMIntPredicate.LLVMIntSLT, first, second, "")!!
        }
        val tt = LLVMBuildSelect(builder, less, Int32(1).llvm, Int32(-1).llvm, "")
        val result = LLVMBuildSelect(builder, equal, Int32(0).llvm, tt, "")
        LLVMBuildRet(builder, result)
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

    private fun castParameters(builder: LLVMBuilderRef, function: IrFunction, useReturnType: Boolean): Pair<LLVMValueRef, LLVMValueRef> {
        val first = codegen.param(function, 0)
        val second = codegen.param(function, 1)
        val unifiedType = if (useReturnType) {
            val llvmFunctionTy = getFunctionType(codegen.llvmFunction(function))
            LLVMGetReturnType(llvmFunctionTy)!!
        } else {
            findUnifiedType(first.type, second.type)
        }
        val firstResult = cast(builder, first, unifiedType)
        val secondResult = cast(builder, second, unifiedType)
        return firstResult to secondResult
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
        if (value.type == floatType && destTy == doubleType) {
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

    private fun binopPrologue(function: IrFunction): LLVMBuilderRef {
        val llvmFunction = codegen.llvmFunction(function)
        val builder = LLVMCreateBuilder()!!
        val bb = LLVMAppendBasicBlock(llvmFunction, "entry")!!
        LLVMPositionBuilderAtEnd(builder, bb)
        return builder
    }
}