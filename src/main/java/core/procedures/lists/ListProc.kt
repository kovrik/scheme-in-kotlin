package core.procedures.lists

import core.procedures.AFn
import core.scm.Cons

class ListProc : AFn() {

    override val isPure = true
    override val name = "list"

    override operator fun invoke(vararg args: Any?): List<Any?>? {
        return Cons.list(*args)
    }
}
