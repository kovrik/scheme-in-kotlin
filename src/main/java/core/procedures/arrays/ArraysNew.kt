package core.procedures.arrays

import core.procedures.AFn

object ArraysNew {

    open class Booleans : AFn<Any?, BooleanArray>(name = "booleans", isPure = true, restArgsType = Boolean::class.javaObjectType) {
        override operator fun invoke(args: Array<out Any?>) = BooleanArray(args.size).apply {
            for (i in 0..args.size - 1) { this[i] = args[i] as Boolean }
        }
    }

    open class Bytes : AFn<Any?, ByteArray>(name = "bytes", isPure = true, restArgsType = Number::class.java) {
        override operator fun invoke(args: Array<out Any?>) = ByteArray(args.size).apply {
            for (i in 0..args.size - 1) { this[i] = (args[i] as Number).toByte() }
        }
    }

    open class Chars : AFn<Any?, CharArray>(name = "chars", isPure = true, restArgsType = Char::class.javaObjectType) {
        override operator fun invoke(args: Array<out Any?>) = CharArray(args.size).apply {
            for (i in 0..args.size - 1) { this[i] = args[i] as Char }
        }
    }

    open class Doubles : AFn<Any?, DoubleArray>(name = "doubles", isPure = true, restArgsType = Number::class.java) {
        override operator fun invoke(args: Array<out Any?>) = DoubleArray(args.size).apply {
            for (i in 0..args.size - 1) { this[i] = (args[i] as Number).toDouble() }
        }
    }

    open class Floats : AFn<Any?, FloatArray>(name = "floats", isPure = true, restArgsType = Number::class.java) {
        override operator fun invoke(args: Array<out Any?>) = FloatArray(args.size).apply {
            for (i in 0..args.size - 1) { this[i] = (args[i] as Number).toFloat() }
        }
    }

    open class Ints : AFn<Any?, IntArray>(name = "ints", isPure = true, restArgsType = Number::class.java) {
        override operator fun invoke(args: Array<out Any?>) = IntArray(args.size).apply {
            for (i in 0..args.size - 1) { this[i] = (args[i] as Number).toInt() }
        }
    }

    open class Longs : AFn<Any?, LongArray>(name = "longs", isPure = true, restArgsType = Number::class.java) {
        override operator fun invoke(args: Array<out Any?>) = LongArray(args.size).apply {
            for (i in 0..args.size - 1) { this[i] = (args[i] as Number).toLong() }
        }
    }

    open class Objects : AFn<Any?, Array<*>>(name = "objects", isPure = true, restArgsType = Any::class.javaObjectType) {
        override operator fun invoke(args: Array<out Any?>) = arrayOfNulls<Any?>(args.size).apply {
            for (i in 0..args.size - 1) { this[i] = args[i] }
        }
    }

    open class Shorts : AFn<Any?, ShortArray>(name = "shorts", isPure = true, restArgsType = Number::class.java) {
        override operator fun invoke(args: Array<out Any?>) = ShortArray(args.size).apply {
            for (i in 0..args.size - 1) { this[i] = (args[i] as Number).toShort() }
        }
    }
}