package core.procedures.seqs

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Type
import core.utils.Utils

class Cycle : AFn<Any?, Any?>(
    name = "cycle", arity = Exactly(1),
    mandatoryArgsTypes = arrayOf(Type.Seqable::class.java)
) {

    override operator fun invoke(arg: Any?): Sequence<Any?> {
        val seq = Utils.toSequence(arg)
        return when (seq.any()) {
            true -> sequence { while (true) yieldAll(seq) }
            false -> emptySequence()
        }
    }
}