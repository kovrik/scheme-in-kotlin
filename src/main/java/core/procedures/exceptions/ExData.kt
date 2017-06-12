package core.procedures.exceptions

import core.exceptions.ExInfoException
import core.procedures.AFn

class ExData : AFn<Any?, Map<*, *>?>(name = "ex-data", isPure = true, minArgs = 1, maxArgs = 1) {

    override operator fun invoke(arg: Any?) = if (arg is ExInfoException) arg.info else null
}
