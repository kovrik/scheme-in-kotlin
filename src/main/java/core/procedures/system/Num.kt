package core.procedures.system

import core.procedures.AFn
import core.procedures.FnArgsBuilder

class Num : AFn(FnArgsBuilder().min(1).max(1).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "num"

    override operator fun invoke(arg: Any?): Number? {
        return arg as Number?
    }
}
