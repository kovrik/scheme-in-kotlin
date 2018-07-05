package core.procedures.system

import core.procedures.AFn
import core.procedures.Arity.Exactly

class Identity : AFn<Any?, Any?>(name = "identity", isPure = true, arity = Exactly(1)) {

    override operator fun invoke(arg: Any?) = arg
}
