package core.procedures.cons

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Cons
import core.scm.Type

class Cdr : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(Type.Pair::class.java)).build()) {

    companion object {

        fun cdr(o: Any?): Any? {
            if (o is Cons<*>) {
                return o.cdr()
            }
            val list = o as List<*>
            return list.subList(1, list.size)
        }
    }

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "cdr"

    override operator fun invoke(arg: Any?): Any? {
        return cdr(arg)
    }
}
