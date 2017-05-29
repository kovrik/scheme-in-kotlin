package core.procedures.system

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.Error

class ErrorProc : AFn(FnArgs(min = 1, max = 1)) {

    override val name = "error"

    override operator fun invoke(arg: Any?): Any? {
        throw Error(arg!!.toString())
    }
}
