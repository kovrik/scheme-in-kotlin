package core.procedures.seqs

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Type
import core.utils.Utils

class Contains : AFn<Any?, Any?>(
    name = "contains?", isPure = true, arity = Exactly(2),
    mandatoryArgsTypes = arrayOf(Type.Seqable::class.java)
) {

    override operator fun invoke(arg1: Any?, arg2: Any?) = Utils.toSequence(arg1).contains(arg2)
}
