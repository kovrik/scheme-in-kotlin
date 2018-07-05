package core.procedures.arrays

import core.procedures.AFn
import core.procedures.Arity.Exactly

object ArraysLength {

    class BytesLength : AFn<ByteArray?, Long>(name = "bytes-length", isPure = true, arity = Exactly(1),
                                              mandatoryArgsTypes = arrayOf(ByteArray::class.java)) {

        override operator fun invoke(arg: ByteArray?) = arg!!.size.toLong()
    }

    class BooleansLength : AFn<BooleanArray?, Long>(name = "booleans-length", isPure = true, arity = Exactly(1),
                                                    mandatoryArgsTypes = arrayOf(BooleanArray::class.java)) {

        override operator fun invoke(arg: BooleanArray?) = arg!!.size.toLong()
    }

    class CharsLength : AFn<CharArray?, Long>(name = "chars-length", isPure = true, arity = Exactly(1),
                                              mandatoryArgsTypes = arrayOf(CharArray::class.java)) {

        override operator fun invoke(arg: CharArray?) = arg!!.size.toLong()
    }

    class DoublesLength : AFn<DoubleArray?, Long>(name = "doubles-length", isPure = true, arity = Exactly(1),
                                                  mandatoryArgsTypes = arrayOf(DoubleArray::class.java)) {

        override operator fun invoke(arg: DoubleArray?) = arg!!.size.toLong()
    }

    class FloatsLength : AFn<FloatArray?, Long>(name = "floats-length", isPure = true, arity = Exactly(1),
                                                mandatoryArgsTypes = arrayOf(FloatArray::class.java)) {

        override operator fun invoke(arg: FloatArray?) = arg!!.size.toLong()
    }

    class IntsLength : AFn<IntArray?, Long>(name = "ints-length", isPure = true, arity = Exactly(1),
                                            mandatoryArgsTypes = arrayOf(IntArray::class.java)) {

        override operator fun invoke(arg: IntArray?) = arg!!.size.toLong()
    }

    class LongsLength : AFn<LongArray?, Long>(name = "longs-length", isPure = true, arity = Exactly(1),
                                              mandatoryArgsTypes = arrayOf(LongArray::class.java)) {

        override operator fun invoke(arg: LongArray?) = arg!!.size.toLong()
    }

    class ObjectsLength : AFn<Array<Any?>?, Long>(name = "objects-length", isPure = true, arity = Exactly(1),
                                                  mandatoryArgsTypes = arrayOf(Array<Any?>::class.java)) {

        override operator fun invoke(arg: Array<Any?>?) = arg!!.size.toLong()
    }

    class ShortsLength : AFn<ShortArray?, Short>(name = "shorts-length", isPure = true, arity = Exactly(1),
                                                 mandatoryArgsTypes = arrayOf(ShortArray::class.java)) {

        override operator fun invoke(arg: ShortArray?) = arg!!.size.toShort()
    }
}