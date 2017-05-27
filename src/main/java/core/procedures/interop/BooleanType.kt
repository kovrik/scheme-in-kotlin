package core.procedures.interop

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.utils.Utils

class BooleanType : AFn(FnArgsBuilder().min(1).max(1).build()) {

    override val isPure = true
    override val name = "boolean"

    override operator fun invoke(arg: Any?): Boolean? {
        when {
            Utils.toBoolean(arg) -> return true
            else -> return false
        }
    }
}
