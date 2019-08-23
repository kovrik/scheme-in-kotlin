package core.procedures.arrays

import core.procedures.AFn

object ArraysAppend {

    class BooleansAppend : AFn<Any?, BooleanArray>(name = "booleans-append", isPure = true, restArgsType = BooleanArray::class.java) {
        override operator fun invoke(args: Array<out Any?>) = mutableListOf<Boolean>().apply {
            args.forEach { arr -> (arr as BooleanArray).forEach { add(it) } }
        }.toBooleanArray()
    }

    class BytesAppend : AFn<Any?, ByteArray>(name = "bytes-append", isPure = true, restArgsType = ByteArray::class.java) {
        override operator fun invoke(args: Array<out Any?>) = mutableListOf<Byte>().apply {
            args.forEach { arr -> (arr as ByteArray).forEach { add(it) } }
        }.toByteArray()
    }

    class CharsAppend : AFn<Any?, CharArray>(name = "chars-append", isPure = true, restArgsType = CharArray::class.java) {
        override operator fun invoke(args: Array<out Any?>) = mutableListOf<Char>().apply {
            for (arr in args) {
                for (b in (arr as CharArray)) {
                    add(b)
                }
            }
        }.toCharArray()
    }

    class DoublesAppend : AFn<Any?, DoubleArray>(name = "doubles-append", isPure = true, restArgsType = DoubleArray::class.java) {
        override operator fun invoke(args: Array<out Any?>) = mutableListOf<Double>().apply {
            for (arr in args) {
                for (b in (arr as DoubleArray)) {
                    add(b)
                }
            }
        }.toDoubleArray()
    }

    class FloatsAppend : AFn<Any?, FloatArray>(name = "floats-append", isPure = true, restArgsType = FloatArray::class.java) {
        override operator fun invoke(args: Array<out Any?>) = mutableListOf<Float>().apply {
            for (arr in args) {
                for (b in (arr as FloatArray)) {
                    add(b)
                }
            }
        }.toFloatArray()
    }

    class IntsAppend : AFn<Any?, IntArray>(name = "ints-append", isPure = true, restArgsType = IntArray::class.java) {
        override operator fun invoke(args: Array<out Any?>) = mutableListOf<Int>().apply {
            for (arr in args) {
                for (b in (arr as IntArray)) {
                    add(b)
                }
            }
        }.toIntArray()
    }

    class LongsAppend : AFn<Any?, LongArray>(name = "longs-append", isPure = true, restArgsType = LongArray::class.java) {
        override operator fun invoke(args: Array<out Any?>) = mutableListOf<Long>().apply {
            for (arr in args) {
                for (b in (arr as LongArray)) {
                    add(b)
                }
            }
        }.toLongArray()
    }

    class ObjectsAppend : AFn<Any?, Array<*>>(name = "objects-append", isPure = true, restArgsType = Array<Any?>::class.java) {
        override operator fun invoke(args: Array<out Any?>) = mutableListOf<Any?>().apply {
            for (arr in args) {
                for (b in (arr as Array<Any?>)) {
                    add(b)
                }
            }
        }.toTypedArray()
    }

    class ShortsAppend : AFn<Any?, ShortArray>(name = "shorts-append", isPure = true, restArgsType = ShortArray::class.java) {
        override operator fun invoke(args: Array<out Any?>) = mutableListOf<Short>().apply {
            for (arr in args) {
                for (b in (arr as ShortArray)) {
                    add(b)
                }
            }
        }.toShortArray()
    }
}
