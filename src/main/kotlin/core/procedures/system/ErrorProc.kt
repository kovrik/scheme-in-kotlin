package core.procedures.system

import core.procedures.AFn
import core.scm.Error

class ErrorProc : AFn<Any?, Unit>(name = "error", minArgs = 1, maxArgs = 1) {

    override operator fun invoke(arg: Any?) = throw Error(arg!!.toString())
}
