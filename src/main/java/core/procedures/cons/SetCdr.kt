package core.procedures.cons

import core.procedures.AFn
import core.scm.Cons
import core.scm.MutablePair
import core.scm.Type

class SetCdr : AFn<Any?, Any?>(name = "set-cdr!", minArgs = 2, maxArgs = 2,
                               mandatoryArgsTypes = arrayOf(Type.Pair::class.java, Any::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?) {
        when (arg1) {
            is MutablePair -> arg1.second = arg2
            else -> {
                val list = arg1 as MutableList<Any?>?
                /* Remove tail */
                list!!.subList(1, list.size).clear()
                /* Set new tail */
                if (arg2 is List<*>) {
                    list.addAll((arg2 as List<*>?)!!)
                } else {
                    list.add(arg2)
                    if (list is Cons<*>) {
                        list.isProperList = false
                    }
                }
            }
        }
    }
}