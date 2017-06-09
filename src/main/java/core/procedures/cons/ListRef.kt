package core.procedures.cons

import core.procedures.AFn
import core.scm.Cons
import core.scm.Type

class ListRef : AFn(name = "list-ref", isPure = true, minArgs = 2, maxArgs = 2,
                    mandatoryArgsTypes = arrayOf(Type.Pair::class.java, Type.ExactNonNegativeInteger::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?): Any? {
        val list = arg1 as List<*>?
        val p = (arg2 as Number).toLong()
        if (p >= list!!.size) {
            throw IndexOutOfBoundsException("$name: value out of range: $p")
        }
        /* Cons cell */
        if (list is Cons<*> && !list.isProperList) {
            if (p == 0L) {
                return list.car()
            }
            throw IllegalArgumentException("$name: index ($p) reaches a non-pair")
        }
        return list[p.toInt()]
    }
}
