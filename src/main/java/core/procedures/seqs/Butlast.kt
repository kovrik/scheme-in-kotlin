package core.procedures.seqs

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Type
import core.utils.Utils

class Butlast : AFn<Any?, Sequence<Any?>?>(
    name = "butlast", isPure = true, arity = Exactly(1),
    mandatoryArgsTypes = arrayOf(Type.Seqable::class.java)
) {

    override operator fun invoke(arg: Any?): Sequence<Any?>? {
        val seq = Utils.toSequence(arg)
        val count = seq.count()
        return when (count > 1) {
            true -> seq.take(count - 1)
            false -> null
        }
    }
}
