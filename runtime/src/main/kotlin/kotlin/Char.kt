/*
 * Copyright 2010-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE file.
 */

package kotlin

import kotlin.native.internal.CoolIntrinsic
import kotlin.native.internal.IntrinsicKind

/**
 * Represents a 16-bit Unicode character.
 */
public class Char private constructor(
        private val value: kotlin.native.internal.ShortValue) : Comparable<Char> {
    /**
     * Compares this value with the specified value for order.
     *
     * Returns zero if this value is equal to the specified other value, a negative number if it's less than other,
     * or a positive number if it's greater than other.
     */
    @CoolIntrinsic(IntrinsicKind.COMPARE_TO)
    external public override fun compareTo(other: Char): Int

    /** Adds the other Int value to this value resulting a Char. */
    @CoolIntrinsic(IntrinsicKind.PLUS)
    external public operator fun plus(other: Int): Char

    /** Subtracts the other Char value from this value resulting an Int. */
    @CoolIntrinsic(IntrinsicKind.MINUS)
    external public operator fun minus(other: Char): Int
    /** Subtracts the other Int value from this value resulting a Char. */
    @CoolIntrinsic(IntrinsicKind.MINUS)
    external public operator fun minus(other: Int): Char

    /** Increments this value. */
    @CoolIntrinsic(IntrinsicKind.INC)
    external public operator fun inc(): Char
    /** Decrements this value. */
    @CoolIntrinsic(IntrinsicKind.DEC)
    external public operator fun dec(): Char

    /** Creates a range from this value to the specified [other] value. */
    public operator fun rangeTo(other: Char): CharRange {
        return CharRange(this, other)
    }

    /** Returns the value of this character as a `Byte`. */
    @CoolIntrinsic(IntrinsicKind.PRIMITIVE_CAST)
    external public fun toByte(): Byte
    /** Returns the value of this character as a `Char`. */
    @CoolIntrinsic(IntrinsicKind.PRIMITIVE_CAST)
    external public fun toChar(): Char
    /** Returns the value of this character as a `Short`. */
    @CoolIntrinsic(IntrinsicKind.PRIMITIVE_CAST)
    external public fun toShort(): Short
    /** Returns the value of this character as a `Int`. */
    @CoolIntrinsic(IntrinsicKind.PRIMITIVE_CAST)
    external public fun toInt(): Int
    /** Returns the value of this character as a `Long`. */
    @CoolIntrinsic(IntrinsicKind.PRIMITIVE_CAST)
    external public fun toLong(): Long
    /** Returns the value of this character as a `Float`. */
    @CoolIntrinsic(IntrinsicKind.PRIMITIVE_CAST)
    external public fun toFloat(): Float
    /** Returns the value of this character as a `Double`. */
    @CoolIntrinsic(IntrinsicKind.PRIMITIVE_CAST)
    external public fun toDouble(): Double

    companion object {
        /**
         * The minimum value of a character code unit.
         */
        @SinceKotlin("1.3")
        public const val MIN_VALUE: Char = '\u0000'

        /**
         * The maximum value of a character code unit.
         */
        @SinceKotlin("1.3")
        public const val MAX_VALUE: Char = '\uFFFF'

        /**
         * The number of bytes used to represent a Char in a binary form.
         */
        @SinceKotlin("1.3")
        public const val SIZE_BYTES: Int = 2
        /**
         * The number of bits used to represent a Char in a binary form.
         */
        @SinceKotlin("1.3")
        public const val SIZE_BITS: Int = 16

        /**
         * The minimum value of a Unicode high-surrogate code unit.
         */
        public const val MIN_HIGH_SURROGATE: Char = '\uD800'

        /**
         * The maximum value of a Unicode high-surrogate code unit.
         */
        public const val MAX_HIGH_SURROGATE: Char = '\uDBFF'

        /**
         * The minimum value of a Unicode low-surrogate code unit.
         */
        public const val MIN_LOW_SURROGATE: Char = '\uDC00'

        /**
         * The maximum value of a Unicode low-surrogate code unit.
         */
        public const val MAX_LOW_SURROGATE: Char = '\uDFFF'

        /**
         * The minimum value of a Unicode surrogate code unit.
         */
        public const val MIN_SURROGATE: Char = MIN_HIGH_SURROGATE

        /**
         * The maximum value of a Unicode surrogate code unit.
         */
        public const val MAX_SURROGATE: Char = MAX_LOW_SURROGATE

        /**
         * The minimum value of a supplementary code point, `\u0x10000`. Kotlin/Native specific.
         */
        public const val MIN_SUPPLEMENTARY_CODE_POINT: Int = 0x10000

        /**
         * The minimum value of a Unicode code point. Kotlin/Native specific.
         */
        public const val MIN_CODE_POINT = 0x000000

        /**
         * The maximum value of a Unicode code point. Kotlin/Native specific.
         */
        public const val MAX_CODE_POINT = 0X10FFFF

        /**
         * The minimum radix available for conversion to and from strings.
         */
        public const val MIN_RADIX: Int = 2

        /**
         * The maximum radix available for conversion to and from strings.
         */
        public const val MAX_RADIX: Int = 36
    }

    public fun equals(other: Char): Boolean = this == other

    public override fun equals(other: Any?): Boolean =
            other is Char && this.equals(other)

    @SymbolName("Kotlin_Char_toString")
    external public override fun toString(): String

    public override fun hashCode(): Int {
        return this.toInt();
    }
}

