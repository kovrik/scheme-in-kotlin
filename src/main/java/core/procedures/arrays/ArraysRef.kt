package core.procedures.arrays

import core.procedures.AFn
import core.scm.Type

object ArraysRef {

    class BytesRef : AFn<Any?, Byte>(name = "bytes-ref", isPure = true, minArgs = 2, maxArgs = 2,
            mandatoryArgsTypes = arrayOf(ByteArray::class.java, Type.ExactNonNegativeInteger::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?): Byte {
            val bytes = arg1 as ByteArray
            val pos = (arg2 as Number).toInt()
            if (pos >= bytes.size) {
                throw IndexOutOfBoundsException("$name: value out of range: $pos")
            }
            return bytes[pos]
        }
    }

    class BooleansRef : AFn<Any?, Boolean>(name = "booleans-ref", isPure = true, minArgs = 2, maxArgs = 2,
            mandatoryArgsTypes = arrayOf(BooleanArray::class.java, Type.ExactNonNegativeInteger::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?): Boolean {
            val booleans = arg1 as BooleanArray
            val pos = (arg2 as Number).toInt()
            if (pos >= booleans.size) {
                throw IndexOutOfBoundsException("$name: value out of range: $pos")
            }
            return booleans[pos]
        }
    }

    class CharsRef : AFn<Any?, Char>(name = "chars-ref", isPure = true, minArgs = 2, maxArgs = 2,
            mandatoryArgsTypes = arrayOf(CharArray::class.java, Type.ExactNonNegativeInteger::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?): Char {
            val chars = arg1 as CharArray
            val pos = (arg2 as Number).toInt()
            if (pos >= chars.size) {
                throw IndexOutOfBoundsException("$name: value out of range: $pos")
            }
            return chars[pos]
        }
    }

    class DoublesRef : AFn<Any?, Double>(name = "doubles-ref", isPure = true, minArgs = 2, maxArgs = 2,
            mandatoryArgsTypes = arrayOf(DoubleArray::class.java, Type.ExactNonNegativeInteger::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?): Double {
            val doubles = arg1 as DoubleArray
            val pos = (arg2 as Number).toInt()
            if (pos >= doubles.size) {
                throw IndexOutOfBoundsException("$name: value out of range: $pos")
            }
            return doubles[pos]
        }
    }

    class FloatsRef : AFn<Any?, Float>(name = "floats-ref", isPure = true, minArgs = 2, maxArgs = 2,
            mandatoryArgsTypes = arrayOf(FloatArray::class.java, Type.ExactNonNegativeInteger::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?): Float {
            val floats = arg1 as FloatArray
            val pos = (arg2 as Number).toInt()
            if (pos >= floats.size) {
                throw IndexOutOfBoundsException("$name: value out of range: $pos")
            }
            return floats[pos]
        }
    }

    class IntsRef : AFn<Any?, Int>(name = "ints-ref", isPure = true, minArgs = 2, maxArgs = 2,
            mandatoryArgsTypes = arrayOf(IntArray::class.java, Type.ExactNonNegativeInteger::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?): Int {
            val ints = arg1 as IntArray
            val pos = (arg2 as Number).toInt()
            if (pos >= ints.size) {
                throw IndexOutOfBoundsException("$name: value out of range: $pos")
            }
            return ints[pos]
        }
    }

    class LongsRef : AFn<Any?, Long>(name = "longs-ref", isPure = true, minArgs = 2, maxArgs = 2,
            mandatoryArgsTypes = arrayOf(LongArray::class.java, Type.ExactNonNegativeInteger::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?): Long {
            val longs = arg1 as LongArray
            val pos = (arg2 as Number).toInt()
            if (pos >= longs.size) {
                throw IndexOutOfBoundsException("$name: value out of range: $pos")
            }
            return longs[pos]
        }
    }

    class ObjectsRef : AFn<Any?, Any?>(name = "objects-ref", isPure = true, minArgs = 2, maxArgs = 2,
            mandatoryArgsTypes = arrayOf(Array<Any?>::class.java, Type.ExactNonNegativeInteger::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?): Any? {
            val objects = arg1 as Array<*>
            val pos = (arg2 as Number).toInt()
            if (pos >= objects.size) {
                throw IndexOutOfBoundsException("$name: value out of range: $pos")
            }
            return objects[pos]
        }
    }

    class ShortsRef : AFn<Any?, Short>(name = "shorts-ref", isPure = true, minArgs = 2, maxArgs = 2,
            mandatoryArgsTypes = arrayOf(ShortArray::class.java, Type.ExactNonNegativeInteger::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?): Short {
            val shorts = arg1 as ShortArray
            val pos = (arg2 as Number).toInt()
            if (pos >= shorts.size) {
                throw IndexOutOfBoundsException("$name: value out of range: $pos")
            }
            return shorts[pos]
        }
    }
}