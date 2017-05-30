package core.procedures.cons

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.Cons
import core.scm.Type

class Car : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(Type.Pair::class.java))) {

    companion object {

        fun car(o: Any?): Any? {
            return when (o) {
                is Cons<*> -> o.car()
                else -> (o as List<*>)[0]
            }
        }
    }

    override val isPure = true
    override val name = "car"

    override operator fun invoke(arg: Any?): Any? {
        return car(arg)
    }
}
