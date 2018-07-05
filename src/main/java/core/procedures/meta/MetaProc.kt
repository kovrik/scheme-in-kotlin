package core.procedures.meta

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.IMeta

class MetaProc : AFn<IMeta, Map<*, *>?>(name = "meta", arity = Exactly(1),
                                        mandatoryArgsTypes = arrayOf(IMeta::class.java)) {

    override operator fun invoke(arg: IMeta) = arg.meta()
}
