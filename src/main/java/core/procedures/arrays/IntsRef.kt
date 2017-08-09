package core.procedures.arrays

import core.procedures.AFn
import core.scm.Type

class IntsRef : AFn<Any?, Int>(name = "ints-ref", isPure = true, minArgs = 2, maxArgs = 2,
                               mandatoryArgsTypes = arrayOf(IntArray::class.java, Type.ExactNonNegativeInteger::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?): Int {
        val ints = arg1 as IntArray
        val pos = (arg2 as Number).toInt()
        if (pos >= ints.size) {
            throw IndexOutOfBoundsException("$name: value out of range: $pos")
        }
        return ints[pos]
    }
}
