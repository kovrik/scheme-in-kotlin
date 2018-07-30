package core.procedures.equivalence

import core.procedures.AFn
import core.procedures.Arity.AtLeast

class Eqv : AFn<Any?, Boolean>(name = "eqv?", isPure = true, arity = AtLeast(2)) {

    override operator fun invoke(arg1: Any?, arg2: Any?) = when {
        arg1 is Collection<*> && arg1.isEmpty() && arg2 is Collection<*> && arg2.isEmpty() -> true
        arg1 is Char && arg2 is Char -> arg1 == arg2
        arg1 is Number && arg2 is Number -> arg1 == arg2
        else -> arg1 === arg2
    }

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        2    -> invoke(args[0], args[1])
        else -> (0..args.size - 2).all { invoke(args[it], args[it + 1]) }
    }
}
