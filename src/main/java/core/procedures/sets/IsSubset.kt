package core.procedures.sets

import core.procedures.AFn

class IsSubset : AFn<Set<*>?, Boolean>(name = "subset?", isPure = true, minArgs = 2, maxArgs = 2,
                     mandatoryArgsTypes = arrayOf<Class<*>>(Set::class.java, Set::class.java)) {

    override operator fun invoke(arg1: Set<*>?, arg2: Set<*>?) = arg2!!.containsAll((arg1!!))
}
