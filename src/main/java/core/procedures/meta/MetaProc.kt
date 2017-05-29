package core.procedures.meta

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.IMeta

class MetaProc : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(IMeta::class.java))) {

    override val name = "meta"

    override operator fun invoke(arg: Any?): Map<*, *>? {
        return (arg as IMeta).meta()
    }
}
