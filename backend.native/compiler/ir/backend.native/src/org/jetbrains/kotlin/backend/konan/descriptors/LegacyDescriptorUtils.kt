/*
 * Copyright 2010-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE file.
 */

package org.jetbrains.kotlin.backend.konan.descriptors

import org.jetbrains.kotlin.backend.common.atMostOne
import org.jetbrains.kotlin.backend.konan.binaryTypeIsReference
import org.jetbrains.kotlin.backend.konan.isObjCClass
import org.jetbrains.kotlin.backend.konan.serialization.isExported
import org.jetbrains.kotlin.builtins.functions.FunctionClassDescriptor
import org.jetbrains.kotlin.builtins.getFunctionalClassKind
import org.jetbrains.kotlin.builtins.isFunctionType
import org.jetbrains.kotlin.descriptors.*
import org.jetbrains.kotlin.descriptors.annotations.AnnotationDescriptor
import org.jetbrains.kotlin.builtins.konan.KonanBuiltIns
import org.jetbrains.kotlin.descriptors.annotations.Annotations
import org.jetbrains.kotlin.descriptors.impl.FunctionDescriptorImpl
import org.jetbrains.kotlin.metadata.konan.KonanProtoBuf
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.resolve.DescriptorFactory
import org.jetbrains.kotlin.resolve.OverridingUtil
import org.jetbrains.kotlin.resolve.constants.StringValue
import org.jetbrains.kotlin.resolve.descriptorUtil.fqNameSafe
import org.jetbrains.kotlin.resolve.descriptorUtil.module
import org.jetbrains.kotlin.resolve.scopes.MemberScope
import org.jetbrains.kotlin.serialization.deserialization.descriptors.DeserializedPropertyDescriptor
import org.jetbrains.kotlin.serialization.deserialization.descriptors.DeserializedSimpleFunctionDescriptor
import org.jetbrains.kotlin.serialization.konan.KonanPackageFragment
import org.jetbrains.kotlin.types.KotlinType
import org.jetbrains.kotlin.types.typeUtil.isUnit
import org.jetbrains.kotlin.util.OperatorNameConventions
import org.jetbrains.kotlin.utils.addToStdlib.ifNotEmpty


/**
 * Implementation of given method.
 *
 * TODO: this method is actually a part of resolve and probably duplicates another one
 */
internal fun <T : CallableMemberDescriptor> T.resolveFakeOverride(): T {
    if (this.kind.isReal) {
        return this
    } else {
        val overridden = OverridingUtil.getOverriddenDeclarations(this)
        val filtered = OverridingUtil.filterOutOverridden(overridden)
        // TODO: is it correct to take first?
        @Suppress("UNCHECKED_CAST")
        return filtered.first { it.modality != Modality.ABSTRACT } as T
    }
}

internal val ClassDescriptor.isArray: Boolean
    get() = this.fqNameSafe.asString() in arrayTypes


internal val ClassDescriptor.isInterface: Boolean
    get() = (this.kind == ClassKind.INTERFACE)

private val kotlinNativeInternalPackageName = FqName.fromSegments(listOf("kotlin", "native", "internal"))

/**
 * @return `konan.internal` member scope
 */
internal val KonanBuiltIns.kotlinNativeInternal: MemberScope
    get() = this.builtInsModule.getPackage(kotlinNativeInternalPackageName).memberScope

internal val KotlinType.isKFunctionType: Boolean
    get() {
        val kind = constructor.declarationDescriptor?.getFunctionalClassKind()
        return kind == FunctionClassDescriptor.Kind.KFunction
    }

internal val FunctionDescriptor.isFunctionInvoke: Boolean
    get() {
        val dispatchReceiver = dispatchReceiverParameter ?: return false
        assert(!dispatchReceiver.type.isKFunctionType)

        return dispatchReceiver.type.isFunctionType &&
                this.isOperator && this.name == OperatorNameConventions.INVOKE
    }

internal fun ClassDescriptor.isUnit() = this.defaultType.isUnit()

internal val <T : CallableMemberDescriptor> T.allOverriddenDescriptors: List<T>
    get() {
        val result = mutableListOf<T>()
        fun traverse(descriptor: T) {
            result.add(descriptor)
            @Suppress("UNCHECKED_CAST")
            descriptor.overriddenDescriptors.forEach { traverse(it as T) }
        }
        traverse(this)
        return result
    }

internal val ClassDescriptor.contributedMethods: List<FunctionDescriptor>
    get () = unsubstitutedMemberScope.contributedMethods

internal val MemberScope.contributedMethods: List<FunctionDescriptor>
    get () {
        val contributedDescriptors = this.getContributedDescriptors()

        val functions = contributedDescriptors.filterIsInstance<FunctionDescriptor>()

        val properties = contributedDescriptors.filterIsInstance<PropertyDescriptor>()
        val getters = properties.mapNotNull { it.getter }
        val setters = properties.mapNotNull { it.setter }

        return functions + getters + setters
    }

fun ClassDescriptor.isAbstract() = this.modality == Modality.SEALED || this.modality == Modality.ABSTRACT
        || this.kind == ClassKind.ENUM_CLASS

internal val FunctionDescriptor.target: FunctionDescriptor
    get() = (if (modality == Modality.ABSTRACT) this else resolveFakeOverride()).original

tailrec internal fun DeclarationDescriptor.findPackage(): PackageFragmentDescriptor {
    return if (this is PackageFragmentDescriptor) this
    else this.containingDeclaration!!.findPackage()
}

internal fun DeclarationDescriptor.findPackageView(): PackageViewDescriptor {
    val packageFragment = this.findPackage()
    return packageFragment.module.getPackage(packageFragment.fqName)
}

internal fun DeclarationDescriptor.allContainingDeclarations(): List<DeclarationDescriptor> {
    var list = mutableListOf<DeclarationDescriptor>()
    var current = this.containingDeclaration
    while (current != null) {
        list.add(current)
        current = current.containingDeclaration
    }
    return list
}

// It is possible to declare "external inline fun",
// but it doesn't have much sense for native,
// since externals don't have IR bodies.
// Enforce inlining of constructors annotated with @InlineConstructor.

private val inlineConstructor = FqName("kotlin.native.internal.InlineConstructor")

internal val FunctionDescriptor.needsInlining: Boolean
    get() {
        val inlineConstructor = annotations.hasAnnotation(inlineConstructor)
        if (inlineConstructor) return true
        return (this.isInline && !this.isExternal)
    }

internal val FunctionDescriptor.needsSerializedIr: Boolean
    get() = (this.needsInlining && this.isExported())

fun AnnotationDescriptor.getStringValueOrNull(name: String): String? {
    val constantValue = this.allValueArguments.entries.atMostOne {
        it.key.asString() == name
    }?.value
    return constantValue?.value as String?
}

fun AnnotationDescriptor.getStringValue(name: String): String = this.getStringValueOrNull(name)!!

private fun getPackagesFqNames(module: ModuleDescriptor): Set<FqName> {
    val result = mutableSetOf<FqName>()

    fun getSubPackages(fqName: FqName) {
        result.add(fqName)
        module.getSubPackagesOf(fqName) { true }.forEach { getSubPackages(it) }
    }

    getSubPackages(FqName.ROOT)
    return result
}

fun ModuleDescriptor.getPackageFragments(): List<PackageFragmentDescriptor> =
        getPackagesFqNames(this).flatMap {
            getPackage(it).fragments.filter { it.module == this }
        }

val ClassDescriptor.enumEntries: List<ClassDescriptor>
    get() {
        assert(this.kind == ClassKind.ENUM_CLASS)
        return this.unsubstitutedMemberScope.getContributedDescriptors()
                .filterIsInstance<ClassDescriptor>()
                .filter { it.kind == ClassKind.ENUM_ENTRY }
    }

internal val DeclarationDescriptor.isExpectMember: Boolean
    get() = this is MemberDescriptor && this.isExpect

internal fun KotlinType?.createExtensionReceiver(owner: CallableDescriptor): ReceiverParameterDescriptor? =
        DescriptorFactory.createExtensionReceiverParameterForCallable(
                owner,
                this,
                Annotations.EMPTY
        )

internal fun FunctionDescriptorImpl.initialize(
        extensionReceiverType: KotlinType?,
        dispatchReceiverParameter: ReceiverParameterDescriptor?,
        typeParameters: List<TypeParameterDescriptor>,
        unsubstitutedValueParameters: List<ValueParameterDescriptor>,
        unsubstitutedReturnType: KotlinType?,
        modality: Modality?,
        visibility: Visibility
): FunctionDescriptorImpl = this.initialize(
        extensionReceiverType.createExtensionReceiver(this),
        dispatchReceiverParameter,
        typeParameters,
        unsubstitutedValueParameters,
        unsubstitutedReturnType,
        modality,
        visibility
)

private fun sourceByIndex(descriptor: CallableMemberDescriptor, index: Int): SourceFile {
    val fragment = descriptor.findPackage() as KonanPackageFragment
    return fragment.sourceFileMap.sourceFile(index)
}

fun CallableMemberDescriptor.findSourceFile(): SourceFile {
    val source = this.source.containingFile
    if (source != SourceFile.NO_SOURCE_FILE)
        return source
    return when {
        this is DeserializedSimpleFunctionDescriptor && proto.hasExtension(KonanProtoBuf.functionFile) -> sourceByIndex(
                this, proto.getExtension(KonanProtoBuf.functionFile))
        this is DeserializedPropertyDescriptor && proto.hasExtension(KonanProtoBuf.propertyFile) ->
            sourceByIndex(
                    this, proto.getExtension(KonanProtoBuf.propertyFile))
        else -> TODO()
    }
}

private val intrinsicAnnotation = FqName("kotlin.native.internal.Intrinsic")
private val symbolNameAnnotation = FqName("kotlin.native.SymbolName")
private val objCMethodAnnotation = FqName("kotlinx.cinterop.ObjCMethod")
private val frozenAnnotation = FqName("kotlin.native.internal.Frozen")

internal val DeclarationDescriptor.isFrozen: Boolean
    get() = this.annotations.hasAnnotation(frozenAnnotation) ||
            (this is org.jetbrains.kotlin.descriptors.ClassDescriptor
                    // RTTI is used for non-reference type box or Objective-C object wrapper:
                    && (!this.defaultType.binaryTypeIsReference() || this.isObjCClass()))

internal val FunctionDescriptor.isIntrinsic: Boolean
    get() = this.annotations.hasAnnotation(intrinsicAnnotation)

// TODO: coalesce all our annotation value getters into fewer functions.
fun getAnnotationValue(annotation: AnnotationDescriptor): String? {
    return annotation.allValueArguments.values.ifNotEmpty {
        val stringValue = single() as? StringValue
        stringValue?.value
    }
}

fun CallableMemberDescriptor.externalSymbolOrThrow(): String? {
    this.annotations.findAnnotation(symbolNameAnnotation)?.let {
        return getAnnotationValue(it)!!
    }
    if (this.annotations.hasAnnotation(intrinsicAnnotation)) return null

    if (this.annotations.hasAnnotation(objCMethodAnnotation)) return null

    throw Error("external function ${this} must have @SymbolName, @Intrinsic or @ObjCMethod annotation")
}
