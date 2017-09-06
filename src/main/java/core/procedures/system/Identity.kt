package core.procedures.system

import core.procedures.AFn

class Identity : AFn<Any?, Any?>(name = "identity", isPure = true, minArgs = 1, maxArgs = 1) {

    override operator fun invoke(arg: Any?) = arg
}
