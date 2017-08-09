package core.procedures.arrays

import core.procedures.AFn
import core.scm.Type

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
