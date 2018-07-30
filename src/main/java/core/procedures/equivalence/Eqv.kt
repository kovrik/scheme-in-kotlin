package core.procedures.equivalence

import core.procedures.AFn
import core.procedures.Arity.AtLeast

class Eqv : AFn<Any?, Boolean>(name = "eqv?", isPure = true, arity = AtLeast(2)) {

    fun eqv(first: Any?, second: Any?) = when {
        first is Collection<*> && first.isEmpty() && second is Collection<*> && second.isEmpty() -> true
        first is Char && second is Char -> first == second
        first is Number && second is Number -> first == second
        else -> first === second
    }

    override operator fun invoke(arg1: Any?, arg2: Any?) = eqv(arg1, arg2)

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        2    -> eqv(args[0], args[1])
        else -> (0..args.size - 2).all { eqv(args[it], args[it + 1]) }
    }
}
