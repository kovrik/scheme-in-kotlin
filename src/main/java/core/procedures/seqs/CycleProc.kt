package core.procedures.seqs

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Type
import core.scm.cycled
import core.utils.Utils

class CycleProc : AFn<Any?, Any?>(
    name = "cycle", arity = Exactly(1),
    mandatoryArgsTypes = arrayOf(Type.Seqable::class.java)
) {

    override operator fun invoke(arg: Any?): Sequence<Any?> = Utils.toSequence(arg).cycled()
}