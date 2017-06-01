package core.procedures.sets

import core.procedures.AFn
import core.procedures.FnArgs

class IsSuperset : AFn(FnArgs(min = 2, max = 2, mandatory =  arrayOf<Class<*>>(Set::class.java, Set::class.java))) {

    override val isPure = true
    override val name = "superset?"
    override operator fun invoke(arg1: Any?, arg2: Any?) = (arg1 as Set<*>).containsAll((arg2 as Set<*>?)!!)
}
