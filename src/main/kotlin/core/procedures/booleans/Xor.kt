package core.procedures.booleans

import core.procedures.AFn

class Xor : AFn<Any?, Any?>(name = "xor", isPure = true, minArgs = 2, maxArgs = 2) {

    override operator fun invoke(arg1: Any?, arg2: Any?): Any? {
        if (arg1 == false && arg2 != false) return arg2
        if (arg2 == false && arg1 != false) return arg1
        return false
    }
}