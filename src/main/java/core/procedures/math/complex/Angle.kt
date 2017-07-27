package core.procedures.math.complex

import core.procedures.AFn
import core.scm.BigComplex
import core.utils.Utils

class Angle : AFn<Number?, Number>(name = "angle", isPure = true, minArgs =  1, maxArgs = 1,
                                   mandatoryArgsTypes = arrayOf<Class<*>>(Number::class.java)) {

    override operator fun invoke(arg: Number?) = angle(arg!!)

    private fun angle(number: Number) = when {
        Utils.isZero(number) -> throw ArithmeticException("$name: undefined for 0")
        else -> BigComplex.of(number).angle()
    }
}
