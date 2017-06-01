package core.procedures.equivalence

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.Cons
import core.scm.Symbol

class Eqv : AFn(FnArgs(min = 2)) {

    companion object {
        fun eqv(first: Any?, second: Any?): Boolean {
            return when {
                first is Char && second is Char       -> first == second
                first is Number && second is Number   -> first == second
                first is Cons<*> && second is Cons<*> -> first === second
                first is List<*> && second is List<*> -> first == second
                first is Symbol && second is Symbol   -> first == second
                else -> first === second
            }
        }
    }

    override val isPure = true
    override val name = "eqv?"
    override operator fun invoke(arg1: Any?, arg2: Any?) = eqv(arg1, arg2)

    override operator fun invoke(vararg args: Any?): Boolean? {
        var result = true
        for (i in 0..args.size - 2) {
            result = result && eqv(args[i], args[i + 1])
        }
        return result
    }
}
