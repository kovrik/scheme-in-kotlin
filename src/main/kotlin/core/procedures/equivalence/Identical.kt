package core.procedures.equivalence

import core.procedures.AFn

class Identical : AFn<Any?, Boolean>(name = "identical?", isPure = true, minArgs = 2) {

    override operator fun invoke(arg1: Any?, arg2: Any?) = arg1 === arg2

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        2    -> invoke(args[0], args[1])
        else -> (0..args.size - 2).all { invoke(args[it], args[it + 1]) }
    }
}
