package core.procedures.math.complex

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Complex

class ImagPart : AFn<Number?, Number>(name = "imag-part", isPure = true, arity = Exactly(1),
                     mandatoryArgsTypes = arrayOf(Number::class.java)) {

    override operator fun invoke(arg: Number?): Number = when (arg) {
        is Complex -> arg.im
        else -> 0L
    }
}
