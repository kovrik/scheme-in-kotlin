package core.procedures.sets

import core.procedures.AFn

class IsSuperset : AFn<Set<*>?, Boolean>(name = "superset?", isPure = true, minArgs = 2, maxArgs = 2,
                       mandatoryArgsTypes =  arrayOf<Class<*>>(Set::class.java, Set::class.java)) {

    override operator fun invoke(arg1: Set<*>?, arg2: Set<*>?) = arg1!!.containsAll((arg2!!))
}
