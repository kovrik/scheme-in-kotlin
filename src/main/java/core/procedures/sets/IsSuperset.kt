package core.procedures.sets

import core.procedures.AFn
import core.procedures.FnArgsBuilder

class IsSuperset : AFn(FnArgsBuilder().min(2).max(2).mandatory(arrayOf<Class<*>>(Set::class.java, Set::class.java)).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "superset?"

    override operator fun invoke(arg1: Any?, arg2: Any?): Boolean {
        return (arg1 as Set<*>).containsAll((arg2 as Set<*>?)!!)
    }
}
