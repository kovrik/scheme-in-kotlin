package core.procedures.arrays

import core.procedures.AFn
import core.scm.Type

open class Subobjects : AFn<Any?, Array<*>>(name = "subobjects", isPure = true, minArgs = 2, maxArgs = 3,
                           mandatoryArgsTypes = arrayOf(Array<Any?>::class.java, Type.ExactNonNegativeInteger::class.java),
                           restArgsType = Type.ExactNonNegativeInteger::class.java) {

    override operator fun invoke(args: Array<out Any?>): Array<*> {
        val objects = args[0] as Array<*>
        val start = (args[1] as Number).toInt()
        if (start > objects.size) {
            throw IndexOutOfBoundsException("$name: value out of range: $start")
        }
        var end = objects.size
        if (args.size == 3) {
            end = (args[2] as Number).toInt()
        }
        if (end > objects.size) {
            throw IndexOutOfBoundsException("$name: value out of range: $end")
        }
        return objects.copyOfRange(start, end)
    }
}
