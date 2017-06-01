package core.procedures.system

import core.procedures.AFn
import core.procedures.FnArgs

class Identity : AFn(FnArgs(min = 1, max = 1)) {

    override val isPure = true
    override val name = "identity"
    override operator fun invoke(arg: Any?) = arg
}
