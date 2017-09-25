package core.procedures.cons

import core.procedures.AFn
import core.procedures.predicates.Predicate
import core.scm.Cons
import core.scm.MutablePair
import core.scm.Type

class Cdr : AFn<Any?, Any?>(name = "cdr", isPure = true, minArgs = 1, maxArgs = 1,
                            mandatoryArgsTypes = arrayOf<Class<*>>(Type.Pair::class.java)) {

    override operator fun invoke(arg: Any?) = when (arg) {
        is Cons<*>     -> if (Predicate.isProperList(arg)) arg.subList(1, arg.size) else (arg.last() as Any)
        is MutablePair -> arg.second
        else           -> (arg as List<*>).subList(1, arg.size)
    }
}
