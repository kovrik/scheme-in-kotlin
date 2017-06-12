package core.procedures.delayed

import core.procedures.AFn
import core.scm.IDeref

class Deref : AFn<IDeref?, Any?>(name = "deref", minArgs = 1, maxArgs = 1, mandatoryArgsTypes = arrayOf<Class<*>>(IDeref::class.java)) {

    override operator fun invoke(arg: IDeref?) = arg!!.deref()
}
