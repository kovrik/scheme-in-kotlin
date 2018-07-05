package core.procedures.arrays

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Type

object ArraysRef {

    class BytesRef : AFn<Any?, Byte>(name = "bytes-ref", isPure = true, arity = Exactly(2),
            mandatoryArgsTypes = arrayOf(ByteArray::class.java, Type.ExactNonNegativeInteger::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?) = (arg1 as ByteArray).let {
            val pos = (arg2 as Number).toInt()
            if (pos >= it.size) throw IndexOutOfBoundsException("$name: value out of range: $pos")
            it[pos]
        }
    }

    class BooleansRef : AFn<Any?, Boolean>(name = "booleans-ref", isPure = true, arity = Exactly(2),
            mandatoryArgsTypes = arrayOf(BooleanArray::class.java, Type.ExactNonNegativeInteger::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?) = (arg1 as BooleanArray).let {
            val pos = (arg2 as Number).toInt()
            if (pos >= it.size) throw IndexOutOfBoundsException("$name: value out of range: $pos")
            it[pos]
        }
    }

    class CharsRef : AFn<Any?, Char>(name = "chars-ref", isPure = true, arity = Exactly(2),
            mandatoryArgsTypes = arrayOf(CharArray::class.java, Type.ExactNonNegativeInteger::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?) = (arg1 as CharArray).let {
            val pos = (arg2 as Number).toInt()
            if (pos >= it.size) throw IndexOutOfBoundsException("$name: value out of range: $pos")
            it[pos]
        }
    }

    class DoublesRef : AFn<Any?, Double>(name = "doubles-ref", isPure = true, arity = Exactly(2),
            mandatoryArgsTypes = arrayOf(DoubleArray::class.java, Type.ExactNonNegativeInteger::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?) = (arg1 as DoubleArray).let {
            val pos = (arg2 as Number).toInt()
            if (pos >= it.size) throw IndexOutOfBoundsException("$name: value out of range: $pos")
            it[pos]
        }
    }

    class FloatsRef : AFn<Any?, Float>(name = "floats-ref", isPure = true, arity = Exactly(2),
            mandatoryArgsTypes = arrayOf(FloatArray::class.java, Type.ExactNonNegativeInteger::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?) = (arg1 as FloatArray).let {
            val pos = (arg2 as Number).toInt()
            if (pos >= it.size) throw IndexOutOfBoundsException("$name: value out of range: $pos")
            it[pos]
        }
    }

    class IntsRef : AFn<Any?, Int>(name = "ints-ref", isPure = true, arity = Exactly(2),
            mandatoryArgsTypes = arrayOf(IntArray::class.java, Type.ExactNonNegativeInteger::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?) = (arg1 as IntArray).let {
            val pos = (arg2 as Number).toInt()
            if (pos >= it.size) throw IndexOutOfBoundsException("$name: value out of range: $pos")
            it[pos]
        }
    }

    class LongsRef : AFn<Any?, Long>(name = "longs-ref", isPure = true, arity = Exactly(2),
            mandatoryArgsTypes = arrayOf(LongArray::class.java, Type.ExactNonNegativeInteger::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?) = (arg1 as LongArray).let {
            val pos = (arg2 as Number).toInt()
            if (pos >= it.size) throw IndexOutOfBoundsException("$name: value out of range: $pos")
            it[pos]
        }
    }

    class ObjectsRef : AFn<Any?, Any?>(name = "objects-ref", isPure = true, arity = Exactly(2),
            mandatoryArgsTypes = arrayOf(Array<Any?>::class.java, Type.ExactNonNegativeInteger::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?) = (arg1 as Array<*>).let {
            val pos = (arg2 as Number).toInt()
            if (pos >= it.size) throw IndexOutOfBoundsException("$name: value out of range: $pos")
            it[pos]
        }
    }

    class ShortsRef : AFn<Any?, Short>(name = "shorts-ref", isPure = true, arity = Exactly(2),
            mandatoryArgsTypes = arrayOf(ShortArray::class.java, Type.ExactNonNegativeInteger::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?) = (arg1 as ShortArray).let {
            val pos = (arg2 as Number).toInt()
            if (pos >= it.size) throw IndexOutOfBoundsException("$name: value out of range: $pos")
            it[pos]
        }
    }
}