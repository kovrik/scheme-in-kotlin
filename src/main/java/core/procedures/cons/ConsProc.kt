package core.procedures.cons

import core.procedures.AFn
import core.scm.Cons

class ConsProc : AFn(name = "cons", minArgs = 2, maxArgs = 2) {

    override operator fun invoke(arg1: Any?, arg2: Any?) = cons(arg1, arg2)

    companion object {
        fun cons(car: Any?, cdr: Any?) = Cons.cons(car, cdr)
    }
}
