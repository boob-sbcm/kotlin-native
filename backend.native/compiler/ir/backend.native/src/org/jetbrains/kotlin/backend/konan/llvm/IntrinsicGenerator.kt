package org.jetbrains.kotlin.backend.konan.llvm

import kotlinx.cinterop.toCValues
import llvm.*
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
    COMPARE_TO
}

internal class IntrinsicGenerator(val codegen: CodeGenerator) {

    val context = codegen.context

    private fun getIntrinsicKind(function: IrFunction): IntrinsicKind {
        val annotation = function.descriptor.annotations.findAnnotation(coolInstrinsicFqName)!!
        val value = annotation.allValueArguments[Name.identifier("kind")]!!.value as Pair<*, Name>
        return IntrinsicKind.valueOf(value.second.asString())
    }

    fun tryGenerate(function: IrFunction): Boolean =
        if (!function.hasAnnotation(coolInstrinsicFqName))
            false
        else when (getIntrinsicKind(function)) {
            IntrinsicKind.PLUS -> emitAdd(function)
            IntrinsicKind.COMPARE_TO -> emitCompareTo(function)
            IntrinsicKind.MINUS -> emitMinus(function)
            IntrinsicKind.TIMES -> emitTimes(function)
            IntrinsicKind.DIV -> emitDiv(function)
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

    private fun cast(builder: LLVMBuilderRef, value: LLVMValueRef, destTy: LLVMTypeRef): LLVMValueRef {
        if (value.type == destTy) return value

        if (value.type == floatType && destTy == doubleType) {
            return LLVMBuildFPExt(builder, value, destTy, "")!!
        }
        if (destTy.isFloatingPoint() && !value.type.isFloatingPoint()) {
            return LLVMBuildSIToFP(builder, value, destTy, "")!!
        }
        return LLVMBuildSExt(builder, value, destTy, "")!!
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

    //TODO: Add always_inline, debug info.
    private fun emitAdd(function: IrFunction): Boolean {
        val builder = binopPrologue(function)

        val (first, second) = castParameters(builder, function, useReturnType=true)

        val result = if (first.type.isFloatingPoint()) {
            LLVMBuildFAdd(builder, first, second, "")
        } else {
            LLVMBuildAdd(builder, first, second, "")
        }
        LLVMBuildRet(builder, result)
        return true
    }

    private fun emitMinus(function: IrFunction): Boolean {
        val builder = binopPrologue(function)

        val (first, second) = castParameters(builder, function, useReturnType=true)

        val result = if (first.type.isFloatingPoint()) {
            LLVMBuildFSub(builder, first, second, "")
        } else {
            LLVMBuildSub(builder, first, second, "")
        }
        LLVMBuildRet(builder, result)
        return true
    }

    private fun emitTimes(function: IrFunction): Boolean {
        val builder = binopPrologue(function)

        val (first, second) = castParameters(builder, function, useReturnType=true)

        val result = if (first.type.isFloatingPoint()) {
            LLVMBuildFMul(builder, first, second, "")
        } else {
            LLVMBuildMul(builder, first, second, "")
        }
        LLVMBuildRet(builder, result)
        return true
    }

    private fun emitDiv(function: IrFunction): Boolean {
        val llvmFunction = codegen.llvmFunction(function)
        val builder = binopPrologue(function)
        val validArgBb = LLVMAppendBasicBlock(llvmFunction, "")!!
        val invalidArgBb = LLVMAppendBasicBlock(llvmFunction, "")!!

        val sp = codegen.param(function, 1)
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
        LLVMBuildCall(builder, throwArthExc, emptyList<LLVMValueRef>().toCValues(), 0, "")
        LLVMBuildUnreachable(builder)

        return true
    }

    private fun emitCompareTo(function: IrFunction): Boolean {
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
        return true
    }

}