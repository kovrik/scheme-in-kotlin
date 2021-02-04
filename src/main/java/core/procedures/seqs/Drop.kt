package core.procedures.seqs

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Type
import core.utils.Utils

class Drop : AFn<Any?, Any?>(
    name = "drop",
    arity = Exactly(2),
    mandatoryArgsTypes = arrayOf(Type.Real::class.java, Type.Seqable::class.java)
) {

    override operator fun invoke(arg1: Any?, arg2: Any?) = Utils.toSequence(arg2).drop((arg1 as Number).toInt())
}
