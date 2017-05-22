package core.procedures.lists

import core.procedures.AFn
import core.scm.Cons

class ListProc : AFn() {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "list"

    override fun apply(args: Array<Any?>): List<Any?>? {
        return Cons.list(*args)
    }
}
