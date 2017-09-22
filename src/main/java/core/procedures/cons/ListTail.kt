package core.procedures.cons

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.scm.Cons
import core.scm.Type

class ListTail : AFn<Any?, Any?>(name = "list-tail", minArgs = 2, maxArgs = 2,
                     mandatoryArgsTypes = arrayOf(Any::class.java, Type.ExactNonNegativeInteger::class.java)) {

    private val cdr = Cdr()

    override operator fun invoke(arg1: Any?, arg2: Any?): Any? {
        val p = (arg2 as Number).toLong()
        if (p == 0L) {
            return arg1
        }
        val list = arg1 as? List<*> ?: throw WrongTypeException(name, "List", arg1)
        if (p >= list.size + 1) {
            throw IndexOutOfBoundsException("$name: value out of range: $p")
        }
        /* Cons cell */
        if (!Cons.isProperList(list)) {
            if (p == 1L) {
                return cdr(list)
            } else {
                throw IndexOutOfBoundsException("$name: value out of range: $p")
            }
        }
        return list.subList(p.toInt(), list.size)
    }
}
