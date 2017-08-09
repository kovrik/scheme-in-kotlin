package core.procedures.generic

import core.procedures.AFn
import core.scm.Cons
import core.scm.Hashmap
import core.scm.MutableVector
import core.scm.Vector

class Empty : AFn<Any?, Any?>(name = "empty", isPure = true, minArgs = 1, maxArgs = 1) {

    override operator fun invoke(arg: Any?): Any? = when (arg) {
        is List<*>   -> Cons.list<Any>()
        is Set<*>    -> HashSet<Any>()
        is Vector    -> MutableVector()
        is ByteArray -> ByteArray(0)
        is Map<*, *> -> Hashmap()
        else         -> null
    }
}
