package core.procedures.math.complex

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.BigComplex

class RealPart : AFn<Number?, Number>(name = "real-part", isPure = true, arity = Exactly(1),
                     mandatoryArgsTypes = arrayOf(Number::class.java)) {

    override operator fun invoke(arg: Number?) = when (arg) {
        is BigComplex -> arg.re
        else -> arg!!
    }
}
