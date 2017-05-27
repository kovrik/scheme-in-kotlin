package core.procedures.meta

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.IMeta

class MetaProc : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(IMeta::class.java)).build()) {

    override val name = "meta"

    override operator fun invoke(arg: Any?): Map<*, *>? {
        return (arg as IMeta).meta()
    }
}
