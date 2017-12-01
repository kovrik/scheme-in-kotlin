package core.procedures.box

import core.procedures.AFn
import core.scm.Box

open class BoxCas : AFn<Any?, Boolean>(name = "box-cas!", minArgs = 3, maxArgs = 3,
                                       mandatoryArgsTypes = arrayOf(Box::class.java, Any::class.java, Any::class.java)) {

    override fun invoke(arg1: Any?, arg2: Any?, arg3: Any?) = (arg1 as Box<Any?>).compareAndSet(arg2, arg3)
}