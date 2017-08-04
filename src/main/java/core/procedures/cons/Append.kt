package core.procedures.cons

import core.procedures.AFn
import core.scm.Cons
import core.scm.Type

class Append : AFn<Any?, Any?>(name = "append", restArgsType = Type.ProperList::class.java, lastArgType = Any::class.java) {

    companion object {

        private val car = Car()
        private val cdr = Cdr()

        fun append(first: Any?, second: Any?): Any? = when {
            Cons.isPair(first) -> Cons.cons(car(first), append(cdr(first), second))
            else               -> second
        }
    }

    override operator fun invoke(args: Array<out Any?>) = args.fold(Cons.EMPTY as Any?, Companion::append)
}
