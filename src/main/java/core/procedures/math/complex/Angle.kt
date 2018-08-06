package core.procedures.math.complex

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Complex
import core.utils.Utils

class Angle : AFn<Number?, Number>(name = "angle", isPure = true, arity = Exactly(1),
                                   mandatoryArgsTypes = arrayOf(Number::class.java)) {

    override operator fun invoke(arg: Number?) = angle(arg!!)

    private fun angle(number: Number) = when {
        Utils.isZero(number) -> throw ArithmeticException("$name: undefined for 0")
        else -> Complex.valueOf(number).angle()
    }
}
