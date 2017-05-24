package core.procedures.cons

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Type
import core.scm.Cons
import core.scm.Void

class SetCdr : AFn(FnArgsBuilder().min(2).max(2).mandatory(arrayOf(Type.Pair::class.java, Any::class.java)).build()) {

    override val name: String
        get() = "set-cdr!"

    override fun apply2(arg1: Any?, arg2: Any?): Any? {
        val list = arg1 as MutableList<Any?>?
        /* Remove tail */
        list!!.subList(1, list.size).clear()
        /* Set new tail */
        if (arg2 is List<*>) {
            list.addAll((arg2 as List<*>?)!!)
        } else {
            list.add(arg2)
            if (list is Cons<*>) {
                list.isList = false
            }
        }
        return Void.VOID
    }
}