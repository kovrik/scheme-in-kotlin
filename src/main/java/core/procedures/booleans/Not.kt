package core.procedures.booleans

import core.procedures.AFn
import core.utils.Utils

class Not : AFn<Any?, Boolean>(name = "not", isPure = true, minArgs = 1, maxArgs = 1) {

    override operator fun invoke(arg: Any?) = !Utils.toBoolean(arg)
}