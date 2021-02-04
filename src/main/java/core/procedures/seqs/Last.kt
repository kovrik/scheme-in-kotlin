package core.procedures.seqs

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Type
import core.utils.Utils

class Last : AFn<Any?, Any?>(
    name = "last", isPure = true, arity = Exactly(1),
    mandatoryArgsTypes = arrayOf(Type.Seqable::class.java)
) {

    override operator fun invoke(arg: Any?) = Utils.toSequence(arg).lastOrNull()
}
