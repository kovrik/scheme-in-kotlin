package core.procedures.equivalence

import core.procedures.AFn

class Eqv : AFn<Any?, Boolean>(name = "eqv?", isPure = true, minArgs = 2) {

    companion object {
        fun eqv(first: Any?, second: Any?): Boolean {
            return when {
                first is List<*> && first.isEmpty() && second is List<*> && second.isEmpty() -> true
                first is Char    && second is Char    -> first == second
                first is Number  && second is Number  -> first == second
                else                                  -> first === second
            }
        }
    }

    override operator fun invoke(arg1: Any?, arg2: Any?) = eqv(arg1, arg2)

    override operator fun invoke(vararg args: Any?): Boolean {
        var result = true
        for (i in 0..args.size - 2) {
            result = result && eqv(args[i], args[i + 1])
        }
        return result
    }
}
