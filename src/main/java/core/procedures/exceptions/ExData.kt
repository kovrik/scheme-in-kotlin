package core.procedures.exceptions

import core.exceptions.ExInfoException
import core.procedures.AFn
import core.procedures.Arity.Exactly

class ExData : AFn<Any?, Map<*, *>?>(name = "ex-data", isPure = true, arity = Exactly(1)) {

    override operator fun invoke(arg: Any?) = (arg as? ExInfoException)?.info
}
