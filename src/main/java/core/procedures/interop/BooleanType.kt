package core.procedures.interop

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.utils.Utils

class BooleanType : AFn<Any?, Boolean>(name = "boolean", isPure = true, arity = Exactly(1)) {

    override operator fun invoke(arg: Any?) = Utils.toBoolean(arg)
}
