package core.procedures.sets

import core.procedures.AFn
import core.procedures.Arity.Exactly

class IsSubset : AFn<Set<*>?, Boolean>(name = "subset?", isPure = true, arity = Exactly(2),
                                       mandatoryArgsTypes = arrayOf(Set::class.java, Set::class.java)) {

    override operator fun invoke(arg1: Set<*>?, arg2: Set<*>?) = arg2!!.containsAll((arg1!!))
}
