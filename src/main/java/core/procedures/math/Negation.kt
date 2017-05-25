package core.procedures.math

import core.procedures.FnArgsBuilder
import core.procedures.AFn
import core.utils.Utils

class Negation : AFn(FnArgsBuilder().min(1).max(1).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "not"

    override operator fun invoke(arg: Any?): Boolean? {
        return !Utils.toBoolean(arg)
    }
}
