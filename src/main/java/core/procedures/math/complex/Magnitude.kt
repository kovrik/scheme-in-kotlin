package core.procedures.math.complex

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.procedures.math.Abs
import core.scm.BigComplex

class Magnitude : AFn<Number?, Number>(name = "magnitude", isPure = true, arity = Exactly(1),
                      mandatoryArgsTypes = arrayOf(Number::class.java)) {

    private val abs = Abs()

    override operator fun invoke(arg: Number?) = when (arg) {
        is BigComplex -> arg.magnitude()
        else -> abs(arg!!)
    }
}
