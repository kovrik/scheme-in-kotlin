package core.procedures.booleans

import core.procedures.AFn
import core.procedures.Arity.Exactly

class Xor : AFn<Any?, Any?>(name = "xor", isPure = true, arity = Exactly(2)) {

    override operator fun invoke(arg1: Any?, arg2: Any?) = when {
        arg1 == false && arg2 != false -> arg2
        arg2 == false && arg1 != false -> arg1
        else -> false
    }
}