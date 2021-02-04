package core.procedures.cons

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.procedures.predicates.Predicate
import core.utils.Utils

class ConsProc : AFn<Any?, Any?>(name = "cons", arity = Exactly(2)) {

    override operator fun invoke(arg1: Any?, arg2: Any?): Any = when (Predicate.isProperList(arg2)) {
        true  -> listOf(arg1) + Utils.toSequence(arg2)
        false -> Pair(arg1, arg2)
    }
}
