package core.procedures.arrays

import core.procedures.AFn
import core.scm.Type

open class Subdoubles : AFn<Any?, DoubleArray>(name = "subdoubles", isPure = true, minArgs = 2, maxArgs = 3,
                           mandatoryArgsTypes = arrayOf(DoubleArray::class.java, Type.ExactNonNegativeInteger::class.java),
                           restArgsType = Type.ExactNonNegativeInteger::class.java) {

    override operator fun invoke(args: Array<out Any?>): DoubleArray {
        val doubles = args[0] as DoubleArray
        val start = (args[1] as Number).toInt()
        if (start > doubles.size) {
            throw IndexOutOfBoundsException("$name: value out of range: $start")
        }
        var end = doubles.size
        if (args.size == 3) {
            end = (args[2] as Number).toInt()
        }
        if (end > doubles.size) {
            throw IndexOutOfBoundsException("$name: value out of range: $end")
        }
        return doubles.copyOfRange(start, end)
    }
}
