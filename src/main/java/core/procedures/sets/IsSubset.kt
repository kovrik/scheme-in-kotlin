package core.procedures.sets

import core.procedures.AFn
import core.procedures.FnArgsBuilder

class IsSubset : AFn(FnArgsBuilder().min(2).max(2).mandatory(arrayOf<Class<*>>(Set::class.java, Set::class.java)).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "subset?"

    override fun apply2(arg1: Any?, arg2: Any?): Boolean {
        return (arg2 as Set<*>).containsAll((arg1 as Set<*>?)!!)
    }
}
