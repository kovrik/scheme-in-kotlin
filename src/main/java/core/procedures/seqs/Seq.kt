package core.procedures.seqs

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.utils.Utils

class Seq : AFn<Any?, Any?>(name = "seq", arity = Exactly(1)) {

    override operator fun invoke(arg: Any?) = Utils.toSequence(arg).let { if (it.iterator().hasNext()) it else null }
}
