package core.procedures.system

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Error

class ErrorProc : AFn(FnArgsBuilder().min(1).max(1).build()) {

    override val name: String
        get() = "error"

    override fun apply1(arg: Any?): Any? {
        throw Error(arg!!.toString())
    }
}
