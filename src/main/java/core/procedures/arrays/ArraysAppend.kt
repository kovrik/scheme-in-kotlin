package core.procedures.arrays

import core.procedures.AFn

object ArraysAppend {

    class BooleansAppend : AFn<Any?, BooleanArray>(name = "booleans-append", isPure = true, restArgsType = BooleanArray::class.java) {
        override operator fun invoke(args: Array<out Any?>) = mutableListOf<Boolean>().apply {
            for (arr in args) {
                addAll((arr as BooleanArray).asSequence())
            }
        }.toBooleanArray()
    }

    class BytesAppend : AFn<Any?, ByteArray>(name = "bytes-append", isPure = true, restArgsType = ByteArray::class.java) {
        override operator fun invoke(args: Array<out Any?>) = mutableListOf<Byte>().apply {
            for (arr in args) {
                addAll((arr as ByteArray).asSequence())
            }
        }.toByteArray()
    }

    class CharsAppend : AFn<Any?, CharArray>(name = "chars-append", isPure = true, restArgsType = CharArray::class.java) {
        override operator fun invoke(args: Array<out Any?>) = mutableListOf<Char>().apply {
            for (arr in args) {
                addAll((arr as CharArray).asSequence())
            }
        }.toCharArray()
    }

    class DoublesAppend : AFn<Any?, DoubleArray>(name = "doubles-append", isPure = true, restArgsType = DoubleArray::class.java) {
        override operator fun invoke(args: Array<out Any?>) = mutableListOf<Double>().apply {
            for (arr in args) {
                addAll((arr as DoubleArray).asSequence())
            }
        }.toDoubleArray()
    }

    class FloatsAppend : AFn<Any?, FloatArray>(name = "floats-append", isPure = true, restArgsType = FloatArray::class.java) {
        override operator fun invoke(args: Array<out Any?>) = mutableListOf<Float>().apply {
            for (arr in args) {
                addAll((arr as FloatArray).asSequence())
            }
        }.toFloatArray()
    }

    class IntsAppend : AFn<Any?, IntArray>(name = "ints-append", isPure = true, restArgsType = IntArray::class.java) {
        override operator fun invoke(args: Array<out Any?>) = mutableListOf<Int>().apply {
            for (arr in args) {
                addAll((arr as IntArray).asSequence())
            }
        }.toIntArray()
    }

    class LongsAppend : AFn<Any?, LongArray>(name = "longs-append", isPure = true, restArgsType = LongArray::class.java) {
        override operator fun invoke(args: Array<out Any?>) = mutableListOf<Long>().apply {
            for (arr in args) {
                addAll((arr as LongArray).asSequence())
            }
        }.toLongArray()
    }

    class ShortsAppend : AFn<Any?, ShortArray>(name = "shorts-append", isPure = true, restArgsType = ShortArray::class.java) {
        override operator fun invoke(args: Array<out Any?>) = mutableListOf<Short>().apply {
            for (arr in args) {
                addAll((arr as ShortArray).asSequence())
            }
        }.toShortArray()
    }

    class ObjectsAppend : AFn<Any?, Array<*>>(name = "objects-append", isPure = true, restArgsType = Array<Any?>::class.java) {
        override operator fun invoke(args: Array<out Any?>) = mutableListOf<Any?>().apply {
            for (arr in args) {
                addAll((arr as Array<Any?>).asSequence())
            }
        }.toTypedArray()
    }
}
