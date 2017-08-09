package core.procedures.arrays

import core.procedures.AFn
import core.scm.Type

class FloatsRef : AFn<Any?, Float>(name = "floats-ref", isPure = true, minArgs = 2, maxArgs = 2,
                                   mandatoryArgsTypes = arrayOf(FloatArray::class.java, Type.ExactNonNegativeInteger::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?): Float {
        val floats = arg1 as FloatArray
        val pos = (arg2 as Number).toInt()
        if (pos >= floats.size) {
            throw IndexOutOfBoundsException("$name: value out of range: $pos")
        }
        return floats[pos]
    }
}
