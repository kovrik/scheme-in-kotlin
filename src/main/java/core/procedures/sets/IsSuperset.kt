package core.procedures.sets

import core.procedures.AFn

class IsSuperset : AFn(name = "superset?", isPure = true, minArgs = 2, maxArgs = 2,
                       mandatoryArgsTypes =  arrayOf<Class<*>>(Set::class.java, Set::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?) = (arg1 as Set<*>).containsAll((arg2 as Set<*>?)!!)
}
