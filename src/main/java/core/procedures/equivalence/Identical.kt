package core.procedures.equivalence

import core.procedures.AFn
import core.procedures.FnArgsBuilder

class Identical : AFn(FnArgsBuilder().min(2).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "identical?"

    override fun apply(vararg args: Any?): Boolean? {
        var result = java.lang.Boolean.TRUE
        for (i in 0..args.size - 2) {
            result = result!! && apply2(args[i], args[i + 1])!!
        }
        return result
    }

    override fun apply2(arg1: Any?, arg2: Any?): Boolean? {
        return arg1 === arg2
    }
}
