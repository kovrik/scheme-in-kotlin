package core.procedures.arrays

import core.procedures.AFn

object ArraysToList {

    class BooleansToList : AFn<BooleanArray?, List<Boolean>?>(name = "booleans->list", isPure = true, minArgs = 1, maxArgs = 1,
            mandatoryArgsTypes = arrayOf<Class<*>>(BooleanArray::class.java)) {

        override operator fun invoke(arg: BooleanArray?) = arg?.toList()
    }

    class BytesToList : AFn<ByteArray?, List<Byte>?>(name = "bytes->list", isPure = true, minArgs = 1, maxArgs = 1,
            mandatoryArgsTypes = arrayOf<Class<*>>(ByteArray::class.java)) {

        override operator fun invoke(arg: ByteArray?) = arg?.toList()
    }

    class CharsToList : AFn<CharArray?, List<Char>?>(name = "chars->list", isPure = true, minArgs = 1, maxArgs = 1,
            mandatoryArgsTypes = arrayOf<Class<*>>(CharArray::class.java)) {

        override operator fun invoke(arg: CharArray?) = arg?.toList()
    }

    class DoublesToList : AFn<DoubleArray?, List<Double>?>(name = "doubles->list", isPure = true, minArgs = 1, maxArgs = 1,
            mandatoryArgsTypes = arrayOf<Class<*>>(DoubleArray::class.java)) {

        override operator fun invoke(arg: DoubleArray?) = arg?.toList()
    }

    class FloatsToList : AFn<FloatArray?, List<Float>?>(name = "floats->list", isPure = true, minArgs = 1, maxArgs = 1,
            mandatoryArgsTypes = arrayOf<Class<*>>(FloatArray::class.java)) {

        override operator fun invoke(arg: FloatArray?) = arg?.toList()
    }

    class IntsToList : AFn<IntArray?, List<Int>?>(name = "ints->list", isPure = true, minArgs = 1, maxArgs = 1,
            mandatoryArgsTypes = arrayOf<Class<*>>(IntArray::class.java)) {

        override operator fun invoke(arg: IntArray?) = arg?.toList()
    }

    class LongsToList : AFn<LongArray?, List<Long>?>(name = "longs->list", isPure = true, minArgs = 1, maxArgs = 1,
            mandatoryArgsTypes = arrayOf<Class<*>>(LongArray::class.java)) {

        override operator fun invoke(arg: LongArray?) = arg?.toList()
    }

    class ShortsToList : AFn<ShortArray?, List<Short>?>(name = "shorts->list", isPure = true, minArgs = 1, maxArgs = 1,
            mandatoryArgsTypes = arrayOf<Class<*>>(ShortArray::class.java)) {

        override operator fun invoke(arg: ShortArray?) = arg?.toList()
    }

    class ObjectsToList : AFn<Array<*>?, List<Any?>?>(name = "objects->list", isPure = true, minArgs = 1, maxArgs = 1,
            mandatoryArgsTypes = arrayOf<Class<*>>(Array<Any?>::class.java)) {

        override operator fun invoke(arg: Array<*>?) = arg?.toList()
    }
}