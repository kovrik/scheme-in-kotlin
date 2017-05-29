package core.procedures.system

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.Void

class Pst : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(Throwable::class.java))) {

    override val name = "pst"

    override operator fun invoke(arg: Any?): Any? {
        (arg as Throwable).printStackTrace()
        return Void
    }
}
