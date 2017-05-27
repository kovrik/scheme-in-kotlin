package core.procedures.cons

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Cons
import core.scm.Type

class Car : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(Type.Pair::class.java)).build()) {

    companion object {

        fun car(o: Any?): Any? {
            when (o) {
                is Cons<*> -> return o.car()
                else -> return (o as List<*>)[0]
            }
        }
    }

    override val isPure = true
    override val name = "car"

    override operator fun invoke(arg: Any?): Any? {
        return car(arg)
    }
}
