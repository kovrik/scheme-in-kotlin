package core.procedures.seqs

import core.procedures.AFn
import core.procedures.IFn
import core.scm.ThunkSeq

open class Iterate : AFn<Any?, Sequence<*>>(name = "iterate", isPure = true, minArgs = 2, maxArgs = 2,
                                            mandatoryArgsTypes = arrayOf<Class<*>>(IFn::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?) = ThunkSeq(generateSequence(arg2, {
        AFn.invokeN((arg1 as IFn<*, *>), arrayOf(it))
    }))
}