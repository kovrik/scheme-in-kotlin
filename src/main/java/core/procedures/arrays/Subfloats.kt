package core.procedures.arrays

import core.procedures.AFn
import core.scm.Type

open class Subfloats : AFn<Any?, FloatArray>(name = "subfloats", isPure = true, minArgs = 2, maxArgs = 3,
                           mandatoryArgsTypes = arrayOf(FloatArray::class.java, Type.ExactNonNegativeInteger::class.java),
                           restArgsType = Type.ExactNonNegativeInteger::class.java) {

    override operator fun invoke(args: Array<out Any?>): FloatArray {
        val floats = args[0] as FloatArray
        val start = (args[1] as Number).toInt()
        if (start > floats.size) {
            throw IndexOutOfBoundsException("$name: value out of range: $start")
        }
        var end = floats.size
        if (args.size == 3) {
            end = (args[2] as Number).toInt()
        }
        if (end > floats.size) {
            throw IndexOutOfBoundsException("$name: value out of range: $end")
        }
        return floats.copyOfRange(start, end)
    }
}
