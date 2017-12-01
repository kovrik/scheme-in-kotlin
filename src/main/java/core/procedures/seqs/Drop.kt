package core.procedures.seqs

import core.procedures.AFn
import core.scm.Type
import core.utils.Utils

class Drop : AFn<Any?, Any?>(name = "drop", minArgs = 2, maxArgs = 2,
                             mandatoryArgsTypes = arrayOf(Type.Real::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?) = Utils.toSequence(arg2).drop((arg1 as Number).toInt())
}
