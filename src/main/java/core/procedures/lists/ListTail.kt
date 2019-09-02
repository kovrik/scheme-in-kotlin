package core.procedures.lists

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.procedures.predicates.Predicate
import core.scm.Type
import core.utils.Utils

class ListTail : AFn<Any?, Any?>(name = "list-tail", arity = Exactly(2),
                                 mandatoryArgsTypes = arrayOf(Any::class.java, Type.ExactNonNegativeInteger::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?): Any? {
        val p = (arg2 as Number).toInt()
        if (p == 0) {
            return arg1
        }
        if (!Predicate.isPairOrNonEmptyList(arg1)) {
            throw WrongTypeException(name, "List", arg1)
        }
        val seq = Utils.toSequence(arg1)
        return when {
            p > seq.count() -> throw IndexOutOfBoundsException("$name: value out of range: $p")
            Predicate.isProperList(arg1) -> seq.drop(p)
            /* Cons cell */
            else -> when (p == 1) {
                true  -> seq.last()
                false -> throw IndexOutOfBoundsException("$name: value out of range: $p")
            }
        }
    }
}
