package core.procedures.seqs

import core.procedures.AFn
import core.scm.ThunkSeq
import core.utils.Utils

class Concat : AFn<Any?, Any?>(name = "concat") {

    override operator fun invoke(args: Array<out Any?>): Sequence<Any?> {
        var seq = emptySequence<Any?>()
        args.forEach { seq = seq.plus(Utils.toSequence(it)) }
        return ThunkSeq(seq)
    }
}

