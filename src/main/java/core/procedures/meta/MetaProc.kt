package core.procedures.meta

import core.procedures.AFn
import core.scm.IMeta

class MetaProc : AFn(name = "meta", minArgs = 1, maxArgs = 1, mandatoryArgsTypes = arrayOf<Class<*>>(IMeta::class.java)) {

    override operator fun invoke(arg: Any?) = (arg as IMeta).meta()
}
