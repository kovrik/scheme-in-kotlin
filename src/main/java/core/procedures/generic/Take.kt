package core.procedures.generic

import core.procedures.AFn
import core.scm.Cons
import core.utils.Utils

class Take : AFn<Any?, Any?>(name = "take", isPure = true, minArgs = 2, maxArgs = 2,
                             mandatoryArgsTypes = arrayOf<Class<*>>(Int::class.javaObjectType)) {

    override operator fun invoke(arg1: Any?, arg2: Any?) = when {
        ((arg1 as Number).toInt()) > 0 -> Utils.toSequence(arg2).take(arg1.toInt()).toCollection(Cons.list())
        else -> Cons.EMPTY
    }
}
