package core.procedures.cons

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.Cons

class ConsProc : AFn(FnArgs(min = 2, max = 2)) {

    companion object {
        fun cons(car: Any?, cdr: Any?): Cons<*> {
            return Cons.cons(car, cdr)
        }
    }

    override val name = "cons"

    override operator fun invoke(arg1: Any?, arg2: Any?): Cons<*>? {
        return cons(arg1, arg2)
    }
}
