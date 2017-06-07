package core.procedures.cons

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.Cons
import core.scm.Type

class SetCdr : AFn(FnArgs(min = 2, max = 2, mandatory = arrayOf(Type.Pair::class.java, Any::class.java))) {

    override val name = "set-cdr!"

    override operator fun invoke(arg1: Any?, arg2: Any?): Unit {
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