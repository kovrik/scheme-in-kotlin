package core.procedures.seqs

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.utils.Utils.toSequence

class First : AFn<Any?, Any?>(name = "first", isPure = true, arity = Exactly(1)) {

    override operator fun invoke(arg: Any?) = toSequence(arg).firstOrNull()
}
