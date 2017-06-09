package core.procedures.equivalence

import core.procedures.AFn
import core.scm.MutableString

class Equal : AFn(name = "equal?", isPure = true, minArgs = 2) {

    override operator fun invoke(vararg args: Any?): Boolean? {
        var result = true
        for (i in 0..args.size - 2) {
            result = result && equal(args[i], args[i + 1])
        }
        return result
    }

    override operator fun invoke(arg1: Any?, arg2: Any?) = equal(arg1, arg2)

    private fun equal(first: Any?, second: Any?) = when {
        first is CharSequence && second is MutableString -> second == first
        else -> first == second
    }
}
