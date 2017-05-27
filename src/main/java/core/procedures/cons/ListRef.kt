package core.procedures.cons

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Cons
import core.scm.Type

class ListRef : AFn(FnArgsBuilder().min(2).max(2)
        .mandatory(arrayOf(Type.Pair::class.java, Type.ExactNonNegativeInteger::class.java)).build()) {

    override val isPure = true
    override val name = "list-ref"

    override operator fun invoke(arg1: Any?, arg2: Any?): Any? {
        val list = arg1 as List<*>?
        val p = (arg2 as Number).toLong()
        if (p >= list!!.size) {
            throw IndexOutOfBoundsException(String.format("%s: value out of range: %s", name, p))
        }
        /* Cons cell */
        if (list is Cons<*> && !list.isList) {
            if (p == 0L) {
                return list.car()
            }
            throw IllegalArgumentException(String.format("%s: index (%s) reaches a non-pair", name, p))
        }
        return list[p.toInt()]
    }
}
