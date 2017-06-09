package core.procedures.cons

import core.procedures.AFn
import core.scm.Cons
import core.scm.Type

class Car : AFn(name = "car", isPure = true, minArgs = 1, maxArgs = 1, mandatoryArgsTypes = arrayOf<Class<*>>(Type.Pair::class.java)) {

    override operator fun invoke(arg: Any?) = car(arg)

    companion object {
        fun car(o: Any?) = when (o) {
            is Cons<*> -> o.car()
            else       -> (o as List<*>)[0]
        }
    }
}
