package core.procedures.lists

import core.procedures.AFn
import core.procedures.cons.Car
import core.procedures.cons.Cdr
import core.procedures.predicates.Predicate
import core.scm.Type
import core.utils.Utils

class Append : AFn<Any?, Any?>(name = "append", restArgsType = Type.ProperList::class.java, lastArgType = Any::class.java) {

    private val car = Car()
    private val cdr = Cdr()

    override operator fun invoke(args: Array<out Any?>) = args.fold(emptyList<Nothing>() as Any?, this::invoke)

    override operator fun invoke(arg1: Any?, arg2: Any?): Any? = when (!Utils.isEmpty(arg1)) {
        true -> {
            val cdr = invoke(cdr(arg1), arg2)
            when (Predicate.isProperList(cdr)) {
                true  -> listOf(car(arg1)) + Utils.toSequence(cdr)
                false -> Pair(car(arg1), cdr)
            }
        }
        false -> arg2
    }
}
