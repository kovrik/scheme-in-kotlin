package core.procedures.arrays

import core.procedures.AFn
import core.scm.Type

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
