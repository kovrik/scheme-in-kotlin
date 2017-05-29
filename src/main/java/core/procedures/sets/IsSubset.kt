package core.procedures.sets

import core.procedures.AFn
import core.procedures.FnArgs

class IsSubset : AFn(FnArgs(min = 2, max = 2, mandatory = arrayOf<Class<*>>(Set::class.java, Set::class.java))) {

    override val isPure = true
    override val name = "subset?"

    override operator fun invoke(arg1: Any?, arg2: Any?): Boolean {
        return (arg2 as Set<*>).containsAll((arg1 as Set<*>?)!!)
    }
}
