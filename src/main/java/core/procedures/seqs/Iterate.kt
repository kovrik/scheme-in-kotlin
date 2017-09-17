package core.procedures.seqs

import core.procedures.AFn
import core.procedures.IFn
import core.scm.LazySeq

open class Iterate : AFn<Any?, Sequence<*>>(name = "iterate", isPure = true, minArgs = 2, maxArgs = 2,
                                            mandatoryArgsTypes = arrayOf<Class<*>>(IFn::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?) = LazySeq(generateSequence(arg2, {
        AFn.invokeN((arg1 as IFn<*, *>), arrayOf(it))
    }))
}