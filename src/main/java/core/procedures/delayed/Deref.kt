package core.procedures.delayed

import core.procedures.AFn
import core.scm.IDeref

class Deref : AFn(name = "deref", minArgs = 1, maxArgs = 1, mandatoryArgsTypes = arrayOf(IDeref::class.java)) {

    override operator fun invoke(arg: Any?) = (arg!! as IDeref).deref()
}
