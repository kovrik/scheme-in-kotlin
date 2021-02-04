package core.procedures.seqs

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Type
import core.utils.Utils

class Seq : AFn<Any?, Any?>(
    name = "seq", arity = Exactly(1),
    mandatoryArgsTypes = arrayOf(Type.Seqable::class.java)
) {

    override operator fun invoke(arg: Any?) = Utils.toSequence(arg).let { if (it.iterator().hasNext()) it else null }
}
