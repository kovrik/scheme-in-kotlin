package core.procedures.equivalence

import core.procedures.AFn
import core.scm.Cons.Companion.EMPTY
import core.scm.Symbol

class Eq : AFn<Any?, Boolean>(name = "eq?", isPure = true, minArgs = 2) {

    override operator fun invoke(args: Array<out Any?>): Boolean {
        var result = true
        for (i in 0..args.size - 2) {
            result = result && eq(args[i], args[i + 1])
        }
        return result
    }

    override operator fun invoke(arg1: Any?, arg2: Any?) = eq(arg1, arg2)

    private fun eq(first: Any?, second: Any?): Boolean {
        if (first is Symbol && second is Symbol) {
            /* Check if 2 symbols are eq ignoring metadata */
            return first == second
        }
        return EMPTY == first && EMPTY == second || first === second
    }
}
