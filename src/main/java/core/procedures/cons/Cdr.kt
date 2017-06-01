package core.procedures.cons

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.Cons
import core.scm.Type

class Cdr : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(Type.Pair::class.java))) {

    companion object {
        fun cdr(o: Any?): Any? {
            if (o is Cons<*>) {
                return o.cdr()
            }
            return (o as List<*>).subList(1, o.size)
        }
    }

    override val isPure = true
    override val name = "cdr"

    override operator fun invoke(arg: Any?) = cdr(arg)
}
