package core.procedures.cons

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.Cons
import core.scm.Type

class ListRef : AFn(FnArgs(min = 2, max = 2, mandatory = arrayOf(Type.Pair::class.java, Type.ExactNonNegativeInteger::class.java))) {

    override val isPure = true
    override val name = "list-ref"

    override operator fun invoke(arg1: Any?, arg2: Any?): Any? {
        val list = arg1 as List<*>?
        val p = (arg2 as Number).toLong()
        if (p >= list!!.size) {
            throw IndexOutOfBoundsException("$name: value out of range: $p")
        }
        /* Cons cell */
        if (list is Cons<*> && !list.isList) {
            if (p == 0L) {
                return list.car()
            }
            throw IllegalArgumentException("$name: index ($p) reaches a non-pair")
        }
        return list[p.toInt()]
    }
}
