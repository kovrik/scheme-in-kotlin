package core.procedures.seqs

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.utils.Utils

open class Count : AFn<Any?, Int>(name = "count", isPure = true, arity = Exactly(1)) {

    override operator fun invoke(arg: Any?) = Utils.toSequence(arg).count()
}
