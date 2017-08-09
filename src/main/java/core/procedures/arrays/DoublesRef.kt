package core.procedures.arrays

import core.procedures.AFn
import core.scm.Type

class DoublesRef : AFn<Any?, Double>(name = "doubles-ref", isPure = true, minArgs = 2, maxArgs = 2,
                                     mandatoryArgsTypes = arrayOf(DoubleArray::class.java, Type.ExactNonNegativeInteger::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?): Double {
        val doubles = arg1 as DoubleArray
        val pos = (arg2 as Number).toInt()
        if (pos >= doubles.size) {
            throw IndexOutOfBoundsException("$name: value out of range: $pos")
        }
        return doubles[pos]
    }
}
