package core.procedures.seqs

import core.procedures.AFn
import core.scm.Cycle
import core.utils.Utils

class CycleProc : AFn<Any?, Any?>(name = "cycle", minArgs = 1, maxArgs = 1) {

    override operator fun invoke(arg: Any?): Sequence<Any?> = Utils.toSequence(arg).let {
        return if (it.iterator().hasNext()) Cycle(it) else emptySequence()
    }
}