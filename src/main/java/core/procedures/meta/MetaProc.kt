package core.procedures.meta

import core.procedures.AFn
import core.scm.IMeta

class MetaProc : AFn<IMeta, Map<*, *>?>(name = "meta", minArgs = 1, maxArgs = 1,
                                        mandatoryArgsTypes = arrayOf(IMeta::class.java)) {

    override operator fun invoke(arg: IMeta) = arg.meta()
}
