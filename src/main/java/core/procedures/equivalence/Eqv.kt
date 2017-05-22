package core.procedures.equivalence

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Cons
import core.scm.Symbol

class Eqv : AFn(FnArgsBuilder().min(2).build()) {

    companion object {
        fun eqv(first: Any?, second: Any?): Boolean {
            when {
                first is Char && second is Char -> return first == second
                first is Number && second is Number -> return first == second
                first is Cons<*> && second is Cons<*> -> return first === second
                first is List<*> && second is List<*> -> return first == second
                first is Symbol && second is Symbol -> return first == second
                else -> return first === second
            }
        }
    }

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "eqv?"

    override fun apply(args: Array<Any?>): Boolean? {
        var result = java.lang.Boolean.TRUE
        for (i in 0..args.size - 2) {
            result = result!! && eqv(args[i], args[i + 1])
        }
        return result
    }

    override fun apply2(arg1: Any?, arg2: Any?): Boolean {
        return eqv(arg1, arg2)
    }
}
