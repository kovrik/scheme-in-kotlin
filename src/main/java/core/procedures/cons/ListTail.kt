package core.procedures.cons

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Cons
import core.scm.Type

class ListTail : AFn(FnArgsBuilder().min(2).max(2)
        .mandatory(arrayOf(Any::class.java, Type.ExactNonNegativeInteger::class.java)).build()) {

    override val name = "list-tail"

    override operator fun invoke(arg1: Any?, arg2: Any?): Any? {
        val p = (arg2 as Number).toLong()
        if (p == 0L) {
            return arg1
        }
        if (arg1 !is List<*>) {
            throw WrongTypeException(name, "List", arg1)
        }
        val list = arg1
        if (p >= list.size + 1) {
            throw IndexOutOfBoundsException(String.format("%s: value out of range: %s", name, p))
        }
        /* Cons cell */
        if (list is Cons<*> && !list.isList) {
            if (p == 1L) {
                return list.cdr()
            } else {
                throw IndexOutOfBoundsException(String.format("%s: value out of range: %s", name, p))
            }
        }
        return list.subList(p.toInt(), list.size)
    }
}
