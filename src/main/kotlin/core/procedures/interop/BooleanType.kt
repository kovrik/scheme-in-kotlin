package core.procedures.interop

import core.procedures.AFn
import core.utils.Utils

class BooleanType : AFn<Any?, Boolean>(name = "boolean", isPure = true, minArgs = 1, maxArgs = 1) {

    override operator fun invoke(arg: Any?) = Utils.toBoolean(arg)
}
