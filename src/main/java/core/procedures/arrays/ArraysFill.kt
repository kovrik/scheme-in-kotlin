package core.procedures.arrays

import core.procedures.AFn
import core.scm.Type

object ArraysFill {

    class BooleansFill : AFn<Any?, BooleanArray>(name = "booleans-fill!", isPure = true, minArgs = 2, maxArgs = 2,
            mandatoryArgsTypes = arrayOf(BooleanArray::class.java, Boolean::class.javaObjectType)) {

        override operator fun invoke(arg1: Any?, arg2: Any?) = (arg1 as BooleanArray).apply { fill(arg2 as Boolean) }
    }

    class BytesFill : AFn<Any?, ByteArray>(name = "bytes-fill!", isPure = true, minArgs = 2, maxArgs = 2,
            mandatoryArgsTypes = arrayOf(ByteArray::class.java, Type.Real::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?) = (arg1 as ByteArray).apply { fill((arg2 as Number).toByte()) }
    }

    class ShortsFill : AFn<Any?, ShortArray>(name = "shorts-fill!", isPure = true, minArgs = 2, maxArgs = 2,
            mandatoryArgsTypes = arrayOf(ShortArray::class.java, Type.Real::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?) = (arg1 as ShortArray).apply { fill((arg2 as Number).toShort()) }
    }

    class CharsFill : AFn<Any?, CharArray>(name = "chars-fill!", isPure = true, minArgs = 2, maxArgs = 2,
            mandatoryArgsTypes = arrayOf(CharArray::class.java, Char::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?) = (arg1 as CharArray).apply { fill(arg2 as Char) }
    }

    class IntsFill : AFn<Any?, IntArray>(name = "ints-fill!", isPure = true, minArgs = 2, maxArgs = 2,
            mandatoryArgsTypes = arrayOf(IntArray::class.java, Type.Real::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?) = (arg1 as IntArray).apply { fill((arg2 as Number).toInt()) }
    }

    class LongsFill : AFn<Any?, LongArray>(name = "longs-fill!", isPure = true, minArgs = 2, maxArgs = 2,
            mandatoryArgsTypes = arrayOf(LongArray::class.java, Type.Real::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?) = (arg1 as LongArray).apply { fill((arg2 as Number).toLong()) }
    }

    class FloatsFill : AFn<Any?, FloatArray>(name = "floats-fill!", isPure = true, minArgs = 2, maxArgs = 2,
            mandatoryArgsTypes = arrayOf(FloatArray::class.java, Type.Real::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?) = (arg1 as FloatArray).apply { fill((arg2 as Number).toFloat()) }
    }

    class DoublesFill : AFn<Any?, DoubleArray>(name = "doubles-fill!", isPure = true, minArgs = 2, maxArgs = 2,
            mandatoryArgsTypes = arrayOf(DoubleArray::class.java, Type.Real::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?) = (arg1 as DoubleArray).apply { fill((arg2 as Number).toDouble()) }
    }

    class ObjectsFill : AFn<Any?, Array<*>>(name = "objects-fill!", isPure = true, minArgs = 2, maxArgs = 2,
            mandatoryArgsTypes = arrayOf(Array<Any?>::class.java, Any::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?) = (arg1 as Array<Any?>).apply { fill(arg2) }
    }
}