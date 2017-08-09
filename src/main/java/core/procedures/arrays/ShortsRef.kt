package core.procedures.arrays

import core.procedures.AFn
import core.scm.Type

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
