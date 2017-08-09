package core.procedures.arrays

import core.procedures.AFn

object ArraysFill {

    class BooleansFill : AFn<Any?, BooleanArray>(name = "booleans-fill!", isPure = true, minArgs = 2, maxArgs = 2,
            mandatoryArgsTypes = arrayOf<Class<*>>(BooleanArray::class.java, Boolean::class.javaObjectType)) {

        override operator fun invoke(arg1: Any?, arg2: Any?): BooleanArray {
            val b = arg2 as Boolean
            return (arg1 as BooleanArray).apply { fill(b) }
        }
    }

    class BytesFill : AFn<Any?, ByteArray>(name = "bytes-fill!", isPure = true, minArgs = 2, maxArgs = 2,
            mandatoryArgsTypes = arrayOf<Class<*>>(ByteArray::class.java, Number::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?): ByteArray {
            val b = (arg2 as Number).toByte()
            return (arg1 as ByteArray).apply { fill(b) }
        }
    }

    class CharsFill : AFn<Any?, CharArray>(name = "chars-fill!", isPure = true, minArgs = 2, maxArgs = 2,
            mandatoryArgsTypes = arrayOf<Class<*>>(CharArray::class.java, Char::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?): CharArray {
            val b = arg2 as Char
            return (arg1 as CharArray).apply { fill(b) }
        }
    }

    class DoublesFill : AFn<Any?, DoubleArray>(name = "doubles-fill!", isPure = true, minArgs = 2, maxArgs = 2,
            mandatoryArgsTypes = arrayOf<Class<*>>(DoubleArray::class.java, Number::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?): DoubleArray {
            val b = (arg2 as Number).toDouble()
            return (arg1 as DoubleArray).apply { fill(b) }
        }
    }

    class FloatsFill : AFn<Any?, FloatArray>(name = "floats-fill!", isPure = true, minArgs = 2, maxArgs = 2,
            mandatoryArgsTypes = arrayOf<Class<*>>(FloatArray::class.java, Number::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?): FloatArray {
            val b = (arg2 as Number).toFloat()
            return (arg1 as FloatArray).apply { fill(b) }
        }
    }

    class IntsFill : AFn<Any?, IntArray>(name = "ints-fill!", isPure = true, minArgs = 2, maxArgs = 2,
            mandatoryArgsTypes = arrayOf<Class<*>>(IntArray::class.java, Number::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?): IntArray {
            val b = (arg2 as Number).toInt()
            return (arg1 as IntArray).apply { fill(b) }
        }
    }

    class LongsFill : AFn<Any?, LongArray>(name = "longs-fill!", isPure = true, minArgs = 2, maxArgs = 2,
            mandatoryArgsTypes = arrayOf<Class<*>>(LongArray::class.java, Number::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?): LongArray {
            val b = (arg2 as Number).toLong()
            return (arg1 as LongArray).apply { fill(b) }
        }
    }

    class ShortsFill : AFn<Any?, ShortArray>(name = "shorts-fill!", isPure = true, minArgs = 2, maxArgs = 2,
            mandatoryArgsTypes = arrayOf<Class<*>>(ShortArray::class.java, Number::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?): ShortArray {
            val b = (arg2 as Number).toShort()
            return (arg1 as ShortArray).apply { fill(b) }
        }
    }

    class ObjectsFill : AFn<Any?, Array<*>>(name = "objects-fill!", isPure = true, minArgs = 2, maxArgs = 2,
            mandatoryArgsTypes = arrayOf<Class<*>>(Array<Any?>::class.java, Any::class.java)) {

        override operator fun invoke(arg1: Any?, arg2: Any?): Array<*> {
            return (arg1 as Array<Any?>).apply { fill(arg2) }
        }
    }
}