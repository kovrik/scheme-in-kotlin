package core.procedures.equivalence

import core.procedures.AFn
import core.scm.MutableString

class Equal : AFn<Any?, Boolean>(name = "equal?", isPure = true, minArgs = 2) {

    override operator fun invoke(args: Array<out Any?>) = (0..args.size - 2).all { equal(args[it], args[it + 1]) }

    override operator fun invoke(arg1: Any?, arg2: Any?) = equal(arg1, arg2)

    private fun equal(first: Any?, second: Any?) = when {
        first is CharSequence && second is MutableString -> second == first
        else -> first == second
    }
}
