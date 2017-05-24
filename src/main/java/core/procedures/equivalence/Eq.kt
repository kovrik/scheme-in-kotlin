package core.procedures.equivalence

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Cons.Companion.EMPTY
import core.scm.Symbol

class Eq : AFn(FnArgsBuilder().min(2).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "eq?"

    override fun apply(args: Array<Any?>): Boolean? {
        var result = java.lang.Boolean.TRUE
        for (i in 0..args.size - 2) {
            result = result!! && eq(args[i], args[i + 1])
        }
        return result
    }

    override fun apply2(arg1: Any?, arg2: Any?): Boolean? {
        return eq(arg1, arg2)
    }

    private fun eq(first: Any?, second: Any?): Boolean {
        if (first is Symbol && second is Symbol && first !== second) {
            /* Now check if 2 symbols are eq without metadata (if attached) */
            return first == second
        }
        return EMPTY == first && EMPTY == second || first === second
    }
}
