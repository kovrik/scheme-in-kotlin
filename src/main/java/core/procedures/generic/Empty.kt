package core.procedures.generic

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.Cons
import core.scm.MutableVector
import core.scm.Vector

import java.util.*

class Empty : AFn(FnArgs(min = 1, max = 1)) {

    override val isPure = true
    override val name = "empty"

    override operator fun invoke(arg: Any?): Any? {
        return when (arg) {
            is List<*>   -> Cons.list<Any>()
            is Set<*>    -> HashSet<Any>()
            is Vector    -> MutableVector()
            is Map<*, *> -> HashMap<Any, Any>()
            else         -> null
        }
    }
}
