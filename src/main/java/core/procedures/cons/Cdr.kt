package core.procedures.cons

import core.procedures.AFn
import core.scm.Cons
import core.scm.Type

class Cdr : AFn<Any?, Any>(name = "cdr", isPure = true, minArgs = 1, maxArgs = 1,
                            mandatoryArgsTypes = arrayOf<Class<*>>(Type.Pair::class.java)) {

    override operator fun invoke(arg: Any?) = cdr(arg)

    companion object {
        fun cdr(o: Any?) = when (o) {
            is Cons<*> -> o.cdr()
            else -> (o as List<*>).subList(1, o.size)
        }
    }
}
