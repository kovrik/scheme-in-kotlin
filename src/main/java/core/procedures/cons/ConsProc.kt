package core.procedures.cons

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Cons

class ConsProc : AFn(FnArgsBuilder().min(2).max(2).build()) {

    companion object {

        fun cons(car: Any?, cdr: Any?): Cons<*> {
            if (car == null && cdr == null) {
                return Cons.EMPTY
            }
            return Cons.cons<Any>(car, cdr)
        }
    }

    override val name: String
        get() = "cons"

    override fun apply2(arg1: Any?, arg2: Any?): Cons<*>? {
        return cons(arg1, arg2)
    }
}
