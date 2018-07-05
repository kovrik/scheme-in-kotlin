package core.procedures.seqs

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.procedures.IFn
import core.scm.ThunkSeq

class Iterate : AFn<Any?, Sequence<*>>(name = "iterate", isPure = true, arity = Exactly(2),
                                       mandatoryArgsTypes = arrayOf(IFn::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?) = ThunkSeq(generateSequence(arg2) {
        AFn.invokeN((arg1 as IFn<*, *>), arrayOf(it))
    })
}