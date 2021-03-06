package core.procedures.cons

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.MutablePair

class Mcdr : AFn<MutablePair<*, *>?, Any?>(name = "mcdr", isPure = true, arity = Exactly(1),
                                     mandatoryArgsTypes = arrayOf(MutablePair::class.java)) {

    override operator fun invoke(arg: MutablePair<*, *>?) = arg!!.second
}
