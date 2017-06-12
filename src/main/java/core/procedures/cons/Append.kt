package core.procedures.cons

import core.procedures.AFn
import core.scm.Cons
import core.scm.Type

class Append : AFn<Any?, Any?>(name = "append", restArgsType = Type.ProperList::class.java, lastArgType = Any::class.java) {

    companion object {
        fun append(first: Any?, second: Any?): Any? {
            if (!Cons.isPair(first)) {
                return second
            }
            return Cons.cons(Car.car(first), append(Cdr.cdr(first), second))
        }
    }

    override operator fun invoke(vararg args: Any?): Any? {
        var result: Any? = Cons.EMPTY
        for (arg in args) {
            result = append(result, arg)
        }
        return result
    }
}
