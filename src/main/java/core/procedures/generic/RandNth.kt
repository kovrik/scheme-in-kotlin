package core.procedures.generic

import core.procedures.AFn
import core.procedures.seqs.Count
import core.procedures.seqs.Get
import core.utils.Utils
import java.util.*

class RandNth : AFn<Any?, Any?>(name = "rand-nth", isPure = true, minArgs = 1, maxArgs = 1) {

    override operator fun invoke(arg: Any?): Any? {
        if (arg is Map<*, *>) {
            throw UnsupportedOperationException("nth not supported on this type: ${arg.javaClass}")
        }
        val seq = Utils.toSequence(arg)
        if (!seq.iterator().hasNext()) {
            throw IndexOutOfBoundsException()
        }
        return seq.elementAt(Random().nextInt(seq.count()))
    }
}

