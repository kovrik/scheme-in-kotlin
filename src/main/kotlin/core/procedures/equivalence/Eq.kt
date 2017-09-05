package core.procedures.equivalence

import core.procedures.AFn
import core.scm.Cons.Companion.EMPTY
import core.scm.Symbol

class Eq : AFn<Any?, Boolean>(name = "eq?", isPure = true, minArgs = 2) {

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        2    -> eq(args[0], args[1])
        else -> (0..args.size - 2).all { eq(args[it], args[it + 1]) }
    }

    override operator fun invoke(arg1: Any?, arg2: Any?) = eq(arg1, arg2)

    private fun eq(first: Any?, second: Any?) = when {
        /* Check if 2 symbols are eq ignoring metadata */
        first is Symbol && second is Symbol -> first == second
        else -> EMPTY == first && EMPTY == second || first === second
    }
}
