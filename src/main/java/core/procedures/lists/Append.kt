package core.procedures.lists

import core.procedures.AFn
import core.procedures.cons.Car
import core.procedures.cons.Cdr
import core.procedures.predicates.Predicate
import core.scm.Cons
import core.scm.Type

class Append : AFn<Any?, Any?>(name = "append", restArgsType = Type.ProperList::class.java, lastArgType = Any::class.java) {

    private val car = Car()
    private val cdr = Cdr()

    override operator fun invoke(args: Array<out Any?>) = args.fold(emptyList<Nothing>() as Any?, this::invoke)

    override operator fun invoke(arg1: Any?, arg2: Any?): Any? = when {
        Predicate.isPair(arg1) -> Cons.cons(car(arg1), invoke(cdr(arg1), arg2))
        else -> arg2
    }
}
