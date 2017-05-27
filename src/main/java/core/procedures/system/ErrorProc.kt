package core.procedures.system

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Error

class ErrorProc : AFn(FnArgsBuilder().min(1).max(1).build()) {

    override val name = "error"

    override operator fun invoke(arg: Any?): Any? {
        throw Error(arg!!.toString())
    }
}
