package core.procedures.lists

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.procedures.cons.Car
import core.procedures.cons.Cdr
import core.procedures.predicates.Predicate
import core.scm.Type
import core.utils.Utils

class ListRef : AFn<Any?, Any?>(name = "list-ref", isPure = true, arity = Exactly(2),
                                mandatoryArgsTypes = arrayOf(Type.PairOrNonEmptyList::class.java,
                                                             Type.ExactNonNegativeInteger::class.java)) {

    private val car = Car()
    private val cdr = Cdr()

    override operator fun invoke(arg1: Any?, arg2: Any?): Any? {
        val p = (arg2 as Number).toLong()
        /* Improper list */
        when {
            !Predicate.isProperList(arg1) -> {
                var cur = arg1
                var i = 0L
                while (!Predicate.isProperList(arg1)) {
                    if (p == i) {
                        return car(cur)
                    }
                    cur = cdr(cur)
                    i += 1
                }
                throw IllegalArgumentException("$name: index ($p) reaches a non-pair")
            }
            else -> {
                val seq = Utils.toSequence(arg1)
                if (p >= seq.count()) {
                    throw IndexOutOfBoundsException("$name: value out of range: $p")
                }
                return seq.drop(p.toInt()).first()
            }
        }
    }
}
