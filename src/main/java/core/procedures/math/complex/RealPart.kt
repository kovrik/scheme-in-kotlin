package core.procedures.math.complex

import core.procedures.AFn
import core.scm.BigComplex

class RealPart : AFn<Number?, Number>(name = "real-part", isPure = true, minArgs = 1, maxArgs = 1,
                     mandatoryArgsTypes = arrayOf<Class<*>>(Number::class.java)) {

    override operator fun invoke(arg: Number?) = when (arg) {
        is BigComplex -> arg.re
        else -> arg!!
    }
}
