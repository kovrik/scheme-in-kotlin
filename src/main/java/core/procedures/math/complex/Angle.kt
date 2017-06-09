package core.procedures.math.complex

import core.procedures.AFn
import core.scm.BigComplex
import core.utils.Utils

class Angle : AFn(name = "angle", isPure = true, minArgs =  1, maxArgs = 1, mandatoryArgsTypes = arrayOf<Class<*>>(Number::class.java)) {

    override operator fun invoke(arg: Any?) = angle(arg!! as Number)

    private fun angle(number: Number): Number {
        if (Utils.isZero(number)) throw ArithmeticException(name + ": undefined for 0")
        return BigComplex.of(number).angle()
    }
}
