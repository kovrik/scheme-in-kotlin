package core.procedures.interop

import core.procedures.AFn
import core.procedures.FnArgs
import core.utils.Utils

class BooleanType : AFn(FnArgs(min = 1, max = 1)) {

    override val isPure = true
    override val name = "boolean"
    override operator fun invoke(arg: Any?) = Utils.toBoolean(arg)
}
