package core.procedures.math

import core.procedures.FnArgs
import core.procedures.AFn
import core.utils.Utils

class Negation : AFn(FnArgs(min = 1, max = 1)) {

    override val isPure = true
    override val name = "not"
    override operator fun invoke(arg: Any?) = !Utils.toBoolean(arg)
}
