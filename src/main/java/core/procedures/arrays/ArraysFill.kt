package core.procedures.arrays

import core.procedures.AFn

object ArraysFill {

    class BooleansFill : AFn<Any?, BooleanArray>(name = "booleans-fill!", isPure = true, minArgs = 2, maxArgs = 2,
            mandatoryArgsTypes = arrayOf<Class<*>>(BooleanArray::class.java, Boolean::class.javaObjectType)) {

        override operator fun invoke(arg1: Any?, arg2: Any?) = (arg1 as BooleanArray).apply { fill(arg2 as Boolean) }
    }

    class BytesFill : AFn<Any?, ByteArray>(name = "bytes-fill!", isPure = true, minArgs = 2, maxArgs = 2,
            mandatoryArgsTypes = arrayOf<Class<*>>(ByteArray::class.java, Number::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?) = (arg1 as ByteArray).apply { fill((arg2 as Number).toByte()) }
    }

    class CharsFill : AFn<Any?, CharArray>(name = "chars-fill!", isPure = true, minArgs = 2, maxArgs = 2,
            mandatoryArgsTypes = arrayOf<Class<*>>(CharArray::class.java, Char::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?) = (arg1 as CharArray).apply { fill(arg2 as Char) }
    }

    class DoublesFill : AFn<Any?, DoubleArray>(name = "doubles-fill!", isPure = true, minArgs = 2, maxArgs = 2,
            mandatoryArgsTypes = arrayOf<Class<*>>(DoubleArray::class.java, Number::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?) = (arg1 as DoubleArray).apply { fill((arg2 as Number).toDouble()) }
    }

    class FloatsFill : AFn<Any?, FloatArray>(name = "floats-fill!", isPure = true, minArgs = 2, maxArgs = 2,
            mandatoryArgsTypes = arrayOf<Class<*>>(FloatArray::class.java, Number::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?) = (arg1 as FloatArray).apply { fill((arg2 as Number).toFloat()) }
    }

    class IntsFill : AFn<Any?, IntArray>(name = "ints-fill!", isPure = true, minArgs = 2, maxArgs = 2,
            mandatoryArgsTypes = arrayOf<Class<*>>(IntArray::class.java, Number::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?) = (arg1 as IntArray).apply { fill((arg2 as Number).toInt()) }
    }

    class LongsFill : AFn<Any?, LongArray>(name = "longs-fill!", isPure = true, minArgs = 2, maxArgs = 2,
            mandatoryArgsTypes = arrayOf<Class<*>>(LongArray::class.java, Number::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?) = (arg1 as LongArray).apply { fill((arg2 as Number).toLong()) }
    }

    class ShortsFill : AFn<Any?, ShortArray>(name = "shorts-fill!", isPure = true, minArgs = 2, maxArgs = 2,
            mandatoryArgsTypes = arrayOf<Class<*>>(ShortArray::class.java, Number::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?) = (arg1 as ShortArray).apply { fill((arg2 as Number).toShort()) }
    }

    class ObjectsFill : AFn<Any?, Array<*>>(name = "objects-fill!", isPure = true, minArgs = 2, maxArgs = 2,
            mandatoryArgsTypes = arrayOf<Class<*>>(Array<Any?>::class.java, Any::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?) = (arg1 as Array<Any?>).apply { fill(arg2) }
    }
}