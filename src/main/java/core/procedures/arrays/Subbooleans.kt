package core.procedures.arrays

import core.procedures.AFn
import core.scm.Type

open class Subbooleans : AFn<Any?, BooleanArray>(name = "subbooleans", isPure = true, minArgs = 2, maxArgs = 3,
                           mandatoryArgsTypes = arrayOf(BooleanArray::class.java, Type.ExactNonNegativeInteger::class.java),
                           restArgsType = Type.ExactNonNegativeInteger::class.java) {

    override operator fun invoke(args: Array<out Any?>): BooleanArray {
        val booleans = args[0] as BooleanArray
        val start = (args[1] as Number).toInt()
        if (start > booleans.size) {
            throw IndexOutOfBoundsException("$name: value out of range: $start")
        }
        var end = booleans.size
        if (args.size == 3) {
            end = (args[2] as Number).toInt()
        }
        if (end > booleans.size) {
            throw IndexOutOfBoundsException("$name: value out of range: $end")
        }
        return booleans.copyOfRange(start, end)
    }
}
