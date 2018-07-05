package core.procedures.seqs

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.utils.Utils
import java.util.*

class RandNth : AFn<Any?, Any?>(name = "rand-nth", isPure = true, arity = Exactly(1)) {

    override operator fun invoke(arg: Any?): Any? {
        if (arg is Map<*, *>) {
            throw UnsupportedOperationException("$name: not supported on this type: ${arg.javaClass}")
        }
        val seq = Utils.toSequence(arg)
        if (!seq.iterator().hasNext()) {
            throw IndexOutOfBoundsException()
        }
        return seq.elementAt(Random().nextInt(seq.count()))
    }
}

