package core.procedures.equivalence

import core.procedures.AFn
import core.procedures.Arity.AtLeast
import core.scm.Symbol

class Eq : AFn<Any?, Boolean>(name = "eq?", isPure = true, arity = AtLeast(2)) {

    private val empty = emptyList<Nothing>()

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        2    -> eq(args[0], args[1])
        else -> (0..args.size - 2).all { eq(args[it], args[it + 1]) }
    }

    override operator fun invoke(arg1: Any?, arg2: Any?) = eq(arg1, arg2)

    private fun eq(first: Any?, second: Any?) = when {
        /* Check if 2 symbols are eq ignoring metadata */
        first is Symbol && second is Symbol -> first == second
        else -> first === second || (empty == first && empty == second)
    }
}
