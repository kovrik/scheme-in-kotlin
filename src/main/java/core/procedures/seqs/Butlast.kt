package core.procedures.seqs

import core.procedures.AFn
import core.utils.Utils

class Butlast : AFn<Any?, Sequence<Any?>?>(name = "butlast", isPure = true, minArgs = 1, maxArgs = 1) {

    override operator fun invoke(arg: Any?): Sequence<Any?>? {
        val seq = Utils.toSequence(arg)
        val count = seq.count()
        return when (count > 1) {
            true  -> seq.take(count - 1)
            false -> null
        }
    }
}
