package core.procedures.equivalence

import core.procedures.AFn
import core.procedures.Arity.AtLeast
import core.scm.Symbol

class Eq : AFn<Any?, Boolean>(name = "eq?", isPure = true, arity = AtLeast(2)) {

    private val empty = emptyList<Nothing>()

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        2    -> invoke(args[0], args[1])
        else -> (0..args.size - 2).all { invoke(args[it], args[it + 1]) }
    }

    override operator fun invoke(arg1: Any?, arg2: Any?) = when {
        /* Check if 2 symbols are eq ignoring metadata */
        arg1 is Symbol && arg2 is Symbol -> arg1 == arg2
        else -> arg1 === arg2 || (empty == arg1 && empty == arg2)
    }
}
