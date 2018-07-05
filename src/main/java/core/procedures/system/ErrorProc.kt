package core.procedures.system

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Error

class ErrorProc : AFn<Any?, Unit>(name = "error", arity = Exactly(1)) {

    override operator fun invoke(arg: Any?) = throw Error(arg!!.toString())
}
