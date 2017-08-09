package core.procedures.arrays

import core.procedures.AFn
import core.scm.Type

open class Subints : AFn<Any?, IntArray>(name = "subints", isPure = true, minArgs = 2, maxArgs = 3,
                           mandatoryArgsTypes = arrayOf(IntArray::class.java, Type.ExactNonNegativeInteger::class.java),
                           restArgsType = Type.ExactNonNegativeInteger::class.java) {

    override operator fun invoke(args: Array<out Any?>): IntArray {
        val ints = args[0] as IntArray
        val start = (args[1] as Number).toInt()
        if (start > ints.size) {
            throw IndexOutOfBoundsException("$name: value out of range: $start")
        }
        var end = ints.size
        if (args.size == 3) {
            end = (args[2] as Number).toInt()
        }
        if (end > ints.size) {
            throw IndexOutOfBoundsException("$name: value out of range: $end")
        }
        return ints.copyOfRange(start, end)
    }
}
