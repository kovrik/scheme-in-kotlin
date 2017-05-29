package core.procedures.system

import core.procedures.AFn
import core.procedures.FnArgs

class Num : AFn(FnArgs(min = 1, max = 1)) {

    override val isPure = true
    override val name = "num"

    override operator fun invoke(arg: Any?): Number? {
        return arg as Number?
    }
}
