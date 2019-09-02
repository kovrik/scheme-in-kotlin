package core.procedures.cons

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Type

class Cdr : AFn<Any?, Any?>(name = "cdr", isPure = true, arity = Exactly(1),
                            mandatoryArgsTypes = arrayOf(Type.PairOrNonEmptyList::class.java)) {

    override operator fun invoke(arg: Any?) = when (arg) {
        is Sequence<*> -> arg.drop(1)
        is Pair<*, *>  -> arg.second
        is List<*>     -> arg.drop(1)
        else           -> throw WrongTypeException(name, Type.ProperList::class.java, arg)
    }
}
