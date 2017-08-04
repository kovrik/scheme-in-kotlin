package core.procedures.cons

import core.procedures.AFn
import core.scm.Cons

class ConsProc : AFn<Any?, List<*>>(name = "cons", minArgs = 2, maxArgs = 2) {

    override operator fun invoke(arg1: Any?, arg2: Any?) = Cons.cons(arg1, arg2)
}
