package core.procedures.math.complex

import core.procedures.AFn
import core.scm.BigComplex

class ImagPart : AFn<Number?, Number>(name = "imag-part", isPure = true, minArgs = 1, maxArgs = 1,
                     mandatoryArgsTypes = arrayOf<Class<*>>(Number::class.java)) {

    override operator fun invoke(arg: Number?): Number = when (arg) {
        is BigComplex -> arg.im
        else -> 0L
    }
}
