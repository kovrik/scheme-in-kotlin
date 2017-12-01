package core.procedures.cons

import core.procedures.AFn
import core.scm.MutablePair

class Mcar : AFn<MutablePair<*, *>?, Any?>(name = "mcar", isPure = true, minArgs = 1, maxArgs = 1,
                                     mandatoryArgsTypes = arrayOf(MutablePair::class.java)) {

    override operator fun invoke(arg: MutablePair<*, *>?) = arg!!.first
}
