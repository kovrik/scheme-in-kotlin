package core.procedures.seqs

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.utils.Utils

class Take : AFn<Any?, Any?>(name = "take", isPure = true, arity = Exactly(2),
                             mandatoryArgsTypes = arrayOf(Int::class.javaObjectType)) {

    override operator fun invoke(arg1: Any?, arg2: Any?) = with(arg1!! as Number) {
        when (toInt() > 0) {
            true  -> Utils.toSequence(arg2).take(toInt())
            false -> emptySequence()
        }
    }
}
