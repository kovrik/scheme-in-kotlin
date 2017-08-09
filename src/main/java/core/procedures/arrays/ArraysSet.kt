package core.procedures.arrays

import core.procedures.AFn
import core.scm.Type

object ArraysSet {

    class BytesSet : AFn<Any?, Unit>(name = "bytes-set!", minArgs = 3, maxArgs = 3,
            mandatoryArgsTypes = arrayOf(ByteArray::class.java, Type.ExactNonNegativeInteger::class.java, Number::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?, arg3: Any?) {
            (arg1!! as ByteArray)[(arg2 as Number).toInt()] = (arg3!! as Number).toByte()
        }
    }

    class BooleansSet : AFn<Any?, Unit>(name = "booleans-set!", minArgs = 3, maxArgs = 3,
            mandatoryArgsTypes = arrayOf(BooleanArray::class.java, Type.ExactNonNegativeInteger::class.java, Boolean::class.javaObjectType)) {

        override operator fun invoke(arg1: Any?, arg2: Any?, arg3: Any?) {
            (arg1!! as BooleanArray)[(arg2 as Number).toInt()] = arg3!! as Boolean
        }
    }

    class CharsSet : AFn<Any?, Unit>(name = "chars-set!", minArgs = 3, maxArgs = 3,
            mandatoryArgsTypes = arrayOf(CharArray::class.java, Type.ExactNonNegativeInteger::class.java, Char::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?, arg3: Any?) {
            (arg1!! as CharArray)[(arg2 as Number).toInt()] = arg3!! as Char
        }
    }

    class DoublesSet : AFn<Any?, Unit>(name = "doubles-set!", minArgs = 3, maxArgs = 3,
            mandatoryArgsTypes = arrayOf(DoubleArray::class.java, Type.ExactNonNegativeInteger::class.java, Number::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?, arg3: Any?) {
            (arg1!! as DoubleArray)[(arg2 as Number).toInt()] = (arg3!! as Number).toDouble()
        }
    }

    class FloatsSet : AFn<Any?, Unit>(name = "floats-set!", minArgs = 3, maxArgs = 3,
            mandatoryArgsTypes = arrayOf(FloatArray::class.java, Type.ExactNonNegativeInteger::class.java, Number::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?, arg3: Any?) {
            (arg1!! as FloatArray)[(arg2 as Number).toInt()] = (arg3!! as Number).toFloat()
        }
    }

    class IntsSet : AFn<Any?, Unit>(name = "ints-set!", minArgs = 3, maxArgs = 3,
            mandatoryArgsTypes = arrayOf(IntArray::class.java, Type.ExactNonNegativeInteger::class.java, Number::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?, arg3: Any?) {
            (arg1!! as IntArray)[(arg2 as Number).toInt()] = (arg3!! as Number).toInt()
        }
    }

    class LongsSet : AFn<Any?, Unit>(name = "longs-set!", minArgs = 3, maxArgs = 3,
            mandatoryArgsTypes = arrayOf(LongArray::class.java, Type.ExactNonNegativeInteger::class.java, Number::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?, arg3: Any?) {
            (arg1!! as LongArray)[(arg2 as Number).toInt()] = (arg3!! as Number).toLong()
        }
    }

    class ShortsSet : AFn<Any?, Unit>(name = "shorts-set!", minArgs = 3, maxArgs = 3,
            mandatoryArgsTypes = arrayOf(ShortArray::class.java, Type.ExactNonNegativeInteger::class.java, Number::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?, arg3: Any?) {
            (arg1!! as ShortArray)[(arg2 as Number).toInt()] = (arg3!! as Number).toShort()
        }
    }

    class ObjectsSet : AFn<Any?, Unit>(name = "objects-set!", minArgs = 3, maxArgs = 3,
            mandatoryArgsTypes = arrayOf(Array<Any?>::class.java, Type.ExactNonNegativeInteger::class.java, Any::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?, arg3: Any?) {
            (arg1!! as Array<Any?>)[(arg2 as Number).toInt()] = arg3
        }
    }
}