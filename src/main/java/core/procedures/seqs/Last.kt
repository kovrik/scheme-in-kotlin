package core.procedures.seqs

import core.procedures.AFn
import core.utils.Utils

class Last : AFn<Any?, Any?>(name = "last", isPure = true, minArgs = 1, maxArgs = 1) {

    override operator fun invoke(arg: Any?): Any? = Utils.toSequence(arg).let { if (it.iterator().hasNext()) it.last() else null }
}
