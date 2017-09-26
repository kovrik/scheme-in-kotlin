package core.procedures.cons

import core.procedures.AFn
import core.scm.MutablePair

class Mcdr : AFn<MutablePair<*, *>?, Any?>(name = "mcdr", isPure = true, minArgs = 1, maxArgs = 1,
                                     mandatoryArgsTypes = arrayOf<Class<*>>(MutablePair::class.java)) {

    override operator fun invoke(arg: MutablePair<*, *>?) = arg!!.second
}
