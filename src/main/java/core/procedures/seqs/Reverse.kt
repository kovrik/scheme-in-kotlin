package core.procedures.seqs

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Type
import core.utils.Utils

class Reverse : AFn<Any?, Any?>(
    name = "reverse", isPure = true, arity = Exactly(1),
    mandatoryArgsTypes = arrayOf(Type.Seqable::class.java)
) {

    override operator fun invoke(arg: Any?): Any = Utils.toSequence(arg).toList().reversed()
}
