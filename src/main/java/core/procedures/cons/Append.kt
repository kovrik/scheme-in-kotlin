package core.procedures.cons

import core.procedures.AFn
import core.scm.Cons
import core.scm.Type

class Append : AFn<Any?, Any?>(name = "append", restArgsType = Type.ProperList::class.java, lastArgType = Any::class.java) {

    companion object {
        fun append(first: Any?, second: Any?): Any? = when {
            Cons.isPair(first) -> Cons.cons(Car.car(first), append(Cdr.cdr(first), second))
            else               -> second
        }
    }

    override operator fun invoke(vararg args: Any?): Any? {
        var result: Any? = Cons.EMPTY
        args.forEach { arg -> result = append(result, arg) }
        return result
    }
}
