package core.procedures.hashmaps

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Cons

class Vals : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(Map::class.java)).build()) {

    override val isPure = true
    override val name = "vals"

    override operator fun invoke(arg: Any?): Any? {
        return Cons.list((arg as Map<Any?, Any?>).values)
    }
}
