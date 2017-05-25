package core.procedures.exceptions

import core.exceptions.ExInfoException
import core.procedures.AFn
import core.procedures.FnArgsBuilder

class ExData : AFn(FnArgsBuilder().min(1).max(1).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "ex-data"

    override operator fun invoke(arg: Any?): Map<*, *>? {
        if (arg is ExInfoException) {
            return arg.info
        }
        return null
    }

}
