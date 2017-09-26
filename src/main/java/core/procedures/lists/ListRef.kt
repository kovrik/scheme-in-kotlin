package core.procedures.lists

import core.procedures.AFn
import core.procedures.cons.Car
import core.procedures.predicates.Predicate
import core.scm.Type

class ListRef : AFn<Any?, Any?>(name = "list-ref", isPure = true, minArgs = 2, maxArgs = 2,
                    mandatoryArgsTypes = arrayOf(Type.Pair::class.java, Type.ExactNonNegativeInteger::class.java)) {

    private val car = Car()

    override operator fun invoke(arg1: Any?, arg2: Any?): Any? {
        val list = arg1 as List<*>?
        val p = (arg2 as Number).toLong()
        if (p >= list!!.size) {
            throw IndexOutOfBoundsException("$name: value out of range: $p")
        }
        /* Cons cell */
        if (!Predicate.isProperList(list)) {
            if (p == 0L) {
                return car(list)
            }
            throw IllegalArgumentException("$name: index ($p) reaches a non-pair")
        }
        return list[p.toInt()]
    }
}
