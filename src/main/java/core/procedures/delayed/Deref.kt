package core.procedures.delayed

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.IDeref

class Deref : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf(IDeref::class.java))) {

    override val name = "deref"

    override operator fun invoke(arg: Any?): Any? {
        return (arg!! as IDeref).deref()
    }
}
