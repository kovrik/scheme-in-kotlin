package core.procedures.equivalence

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.MutableString

class Equal : AFn(FnArgs(min = 2)) {

    override val isPure = true
    override val name = "equal?"

    override operator fun invoke(vararg args: Any?): Boolean? {
        var result = true
        for (i in 0..args.size - 2) {
            result = result && equal(args[i], args[i + 1])
        }
        return result
    }

    override operator fun invoke(arg1: Any?, arg2: Any?): Boolean {
        return equal(arg1, arg2)
    }

    private fun equal(first: Any?, second: Any?): Boolean {
        if (first is CharSequence && second is MutableString) {
            return second == first
        }
        return first == second
    }
}
