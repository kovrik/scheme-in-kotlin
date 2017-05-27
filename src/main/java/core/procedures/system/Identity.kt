package core.procedures.system

import core.procedures.AFn
import core.procedures.FnArgsBuilder

class Identity : AFn(FnArgsBuilder().min(1).max(1).build()) {

    override val isPure = true
    override val name = "identity"

    override operator fun invoke(arg: Any?): Any? {
        return arg
    }
}
