package core.procedures.cons

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.scm.Cons
import core.scm.Type

class ListTail : AFn<Any?, Any?>(name = "list-tail", minArgs = 2, maxArgs = 2,
                     mandatoryArgsTypes = arrayOf(Any::class.java, Type.ExactNonNegativeInteger::class.java)) {

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
            throw IndexOutOfBoundsException("$name: value out of range: $p")
        }
        /* Cons cell */
        if (list is Cons<*> && !list.isProperList) {
            if (p == 1L) {
                return list.cdr()
            } else {
                throw IndexOutOfBoundsException("$name: value out of range: $p")
            }
        }
        return list.subList(p.toInt(), list.size)
    }
}
