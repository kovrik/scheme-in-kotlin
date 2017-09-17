package core.procedures.seqs

import core.procedures.AFn
import core.scm.LazySeq
import core.utils.Utils

class Cycle : AFn<Any?, Any?>(name = "cycle", minArgs = 1, maxArgs = 1) {

    // TODO make it return emptySequence if seq is empty!
    override operator fun invoke(arg: Any?): Sequence<Any?> {
        val seq = Utils.toSequence(arg)
        return LazySeq(seq.let { generateSequence(it) { it }.flatten() })
    }
}