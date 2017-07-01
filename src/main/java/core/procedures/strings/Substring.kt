package core.procedures.strings

import core.procedures.AFn
import core.scm.Type

open class Substring : AFn<Any?, String>(name = "substring", isPure = true, minArgs = 2, maxArgs = 3,
                           mandatoryArgsTypes = arrayOf(CharSequence::class.java, Type.ExactNonNegativeInteger::class.java),
                           restArgsType = Type.ExactNonNegativeInteger::class.java) {

    override operator fun invoke(args: Array<Any?>): String {
        val s = args[0].toString()
        val start = (args[1] as Number).toInt()
        if (start > s.length) {
            throw IndexOutOfBoundsException("$name: value out of range: $start")
        }
        var end = s.length
        if (args.size == 3) {
            end = (args[2] as Number).toInt()
        }
        if (end > s.length) {
            throw IndexOutOfBoundsException("$name: value out of range: $end")
        }
        return s.substring(start, end)
    }
}
