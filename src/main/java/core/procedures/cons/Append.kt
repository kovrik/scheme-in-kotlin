package core.procedures.cons

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Cons
import core.scm.Type

class Append : AFn(FnArgsBuilder().rest(Type.ProperList::class.java).last(Any::class.java).build()) {

    companion object {

        fun append(first: Any?, second: Any?): Any? {
            if (Cons.isNull(first)) {
                return second
            }
            return Cons.cons(Car.car(first), append(Cdr.cdr(first), second))
        }
    }

    override val name: String
        get() = "append"

    override fun apply(vararg args: Any?): Any? {
        var result: Any? = Cons.EMPTY
        for (arg in args) {
            result = append(result, arg)
        }
        return result
    }
}
