package core.procedures.equivalence

import core.procedures.AFn

class Identical : AFn(name = "identical?", isPure = true, minArgs = 2) {

    override operator fun invoke(arg1: Any?, arg2: Any?) = arg1 === arg2

    override operator fun invoke(vararg args: Any?): Boolean? {
        var result = true
        for (i in 0..args.size - 2) {
            result = result && invoke(args[i], args[i + 1])
        }
        return result
    }
}
