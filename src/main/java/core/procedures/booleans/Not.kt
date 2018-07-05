package core.procedures.booleans

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.utils.Utils

class Not : AFn<Any?, Boolean>(name = "not", isPure = true, arity = Exactly(1)) {

    override operator fun invoke(arg: Any?) = !Utils.toBoolean(arg)
}
