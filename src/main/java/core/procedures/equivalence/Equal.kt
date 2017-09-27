package core.procedures.equivalence

import core.procedures.AFn
import core.scm.Cons
import core.scm.MutableString

class Equal : AFn<Any?, Boolean>(name = "equal?", isPure = true, minArgs = 2) {

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        2    -> equal(args[0], args[1])
        else -> (0..args.size - 2).all { equal(args[it], args[it + 1]) }
    }

    override operator fun invoke(arg1: Any?, arg2: Any?) = equal(arg1, arg2)

    private fun equal(first: Any?, second: Any?) = when {
        first is List<*> && second is Cons<*> -> second == first
        first is CharSequence && second is MutableString -> second == first
        else -> first == second
    }
}
