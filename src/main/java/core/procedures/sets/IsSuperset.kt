package core.procedures.sets

import core.procedures.AFn
import core.procedures.Arity.Exactly

class IsSuperset : AFn<Set<*>?, Boolean>(name = "superset?", isPure = true, arity = Exactly(2),
                                         mandatoryArgsTypes =  arrayOf(Set::class.java, Set::class.java)) {

    override operator fun invoke(arg1: Set<*>?, arg2: Set<*>?) = arg1!!.containsAll((arg2!!))
}
