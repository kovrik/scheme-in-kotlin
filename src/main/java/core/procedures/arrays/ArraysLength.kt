package core.procedures.arrays

import core.procedures.AFn

object ArraysLength {

    class BytesLength : AFn<ByteArray?, Long>(name = "bytes-length", isPure = true, minArgs = 1, maxArgs = 1,
            mandatoryArgsTypes = arrayOf<Class<*>>(ByteArray::class.java)) {

        override operator fun invoke(arg: ByteArray?) = arg!!.size.toLong()
    }

    class BooleansLength : AFn<BooleanArray?, Long>(name = "booleans-length", isPure = true, minArgs = 1, maxArgs = 1,
            mandatoryArgsTypes = arrayOf<Class<*>>(BooleanArray::class.java)) {

        override operator fun invoke(arg: BooleanArray?) = arg!!.size.toLong()
    }

    class CharsLength : AFn<CharArray?, Long>(name = "chars-length", isPure = true, minArgs = 1, maxArgs = 1,
            mandatoryArgsTypes = arrayOf<Class<*>>(CharArray::class.java)) {

        override operator fun invoke(arg: CharArray?) = arg!!.size.toLong()
    }

    class DoublesLength : AFn<DoubleArray?, Long>(name = "doubles-length", isPure = true, minArgs = 1, maxArgs = 1,
            mandatoryArgsTypes = arrayOf<Class<*>>(DoubleArray::class.java)) {

        override operator fun invoke(arg: DoubleArray?) = arg!!.size.toLong()
    }

    class FloatsLength : AFn<FloatArray?, Long>(name = "floats-length", isPure = true, minArgs = 1, maxArgs = 1,
            mandatoryArgsTypes = arrayOf<Class<*>>(FloatArray::class.java)) {

        override operator fun invoke(arg: FloatArray?) = arg!!.size.toLong()
    }

    class IntsLength : AFn<IntArray?, Long>(name = "ints-length", isPure = true, minArgs = 1, maxArgs = 1,
            mandatoryArgsTypes = arrayOf<Class<*>>(IntArray::class.java)) {

        override operator fun invoke(arg: IntArray?) = arg!!.size.toLong()
    }

    class LongsLength : AFn<LongArray?, Long>(name = "longs-length", isPure = true, minArgs = 1, maxArgs = 1,
            mandatoryArgsTypes = arrayOf<Class<*>>(LongArray::class.java)) {

        override operator fun invoke(arg: LongArray?) = arg!!.size.toLong()
    }

    class ObjectsLength : AFn<Array<Any?>?, Long>(name = "objects-length", isPure = true, minArgs = 1, maxArgs = 1,
            mandatoryArgsTypes = arrayOf<Class<*>>(Array<Any?>::class.java)) {

        override operator fun invoke(arg: Array<Any?>?) = arg!!.size.toLong()
    }

    class ShortsLength : AFn<ShortArray?, Short>(name = "shorts-length", isPure = true, minArgs = 1, maxArgs = 1,
            mandatoryArgsTypes = arrayOf<Class<*>>(ShortArray::class.java)) {

        override operator fun invoke(arg: ShortArray?) = arg!!.size.toShort()
    }
}