package core.procedures.seqs

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Cycle
import core.utils.Utils

class CycleProc : AFn<Any?, Any?>(name = "cycle", arity = Exactly(1)) {

    override operator fun invoke(arg: Any?): Sequence<Any?> = Utils.toSequence(arg).let {
        return if (it.iterator().hasNext()) Cycle(it) else emptySequence()
    }
}