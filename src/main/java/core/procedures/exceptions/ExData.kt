package core.procedures.exceptions

import core.exceptions.ExInfoException
import core.procedures.AFn
import core.procedures.FnArgs

class ExData : AFn(FnArgs(min = 1, max = 1)) {

    override val isPure = true
    override val name = "ex-data"

    override operator fun invoke(arg: Any?): Map<*, *>? {
        if (arg is ExInfoException) {
            return arg.info
        }
        return null
    }
}
