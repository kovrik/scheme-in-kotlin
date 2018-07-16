package core.procedures.lists

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.procedures.cons.Cdr
import core.procedures.predicates.Predicate
import core.scm.Type

class ListTail : AFn<Any?, Any?>(name = "list-tail", arity = Exactly(2),
                     mandatoryArgsTypes = arrayOf(Any::class.java, Type.ExactNonNegativeInteger::class.java)) {

    private val cdr = Cdr()

    override operator fun invoke(arg1: Any?, arg2: Any?): Any? {
        val p = (arg2 as Number).toInt()
        if (p == 0) {
            return arg1
        }
        val list = arg1 as? List<*> ?: throw WrongTypeException(name, "List", arg1)
        return when {
            p > list.size -> throw IndexOutOfBoundsException("$name: value out of range: $p")
            Predicate.isProperList(list) -> list.drop(p)
        /* Cons cell */
            else -> when (p == 1) {
                true -> cdr(list)
                false -> throw IndexOutOfBoundsException("$name: value out of range: $p")
            }
        }
    }
}
