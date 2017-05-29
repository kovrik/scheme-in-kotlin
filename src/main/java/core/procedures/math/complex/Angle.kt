package core.procedures.math.complex

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.BigComplex
import core.utils.Utils
import java.lang.NullPointerException

class Angle : AFn(FnArgs(min =  1, max = 1, mandatory =  arrayOf<Class<*>>(Number::class.java))) {

    override val isPure = true
    override val name = "angle"

    override operator fun invoke(arg: Any?): Number? {
        if (arg == null) throw NullPointerException()
        return angle(arg as Number)
    }

    private fun angle(number: Number): Number {
        if (Utils.isZero(number)) throw ArithmeticException(name + ": undefined for 0")
        return BigComplex.of(number).angle()
    }
}
