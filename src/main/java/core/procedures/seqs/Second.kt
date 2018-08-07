package core.procedures.seqs

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.utils.Utils

class Second : AFn<Any?, Any?>(name = "second", isPure = true, arity = Exactly(1)) {

    override operator fun invoke(arg: Any?) = Utils.toSequence(arg).elementAtOrNull(1)
}
