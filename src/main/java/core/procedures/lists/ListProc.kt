package core.procedures.lists

import core.procedures.AFn
import core.scm.Cons

class ListProc : AFn<Any?, List<*>>(name = "list", isPure = true) {

    override operator fun invoke(vararg args: Any?) = Cons.list(*args)
}
