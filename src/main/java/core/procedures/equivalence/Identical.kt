package core.procedures.equivalence

import core.procedures.AFn
import core.procedures.FnArgsBuilder

class Identical : AFn(FnArgsBuilder().min(2).build()) {

    override val isPure = true
    override val name = "identical?"

    override operator fun invoke(vararg args: Any?): Boolean? {
        var result = java.lang.Boolean.TRUE
        for (i in 0..args.size - 2) {
            result = result!! && invoke(args[i], args[i + 1])!!
        }
        return result
    }

    override operator fun invoke(arg1: Any?, arg2: Any?): Boolean? {
        return arg1 === arg2
    }
}
