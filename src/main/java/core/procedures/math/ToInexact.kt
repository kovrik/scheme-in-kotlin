package core.procedures.math

import core.procedures.AFn
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils
import java.math.BigDecimal
import java.math.BigInteger

class ToInexact : AFn(name = "exact->inexact", isPure = true, minArgs = 1, maxArgs = 1, mandatoryArgsTypes = arrayOf<Class<*>>(Number::class.java)) {

    override operator fun invoke(arg: Any?) = toInexact(arg)

    companion object {
        fun toInexact(o: Any?): Number = when (o) {
            is BigComplex -> BigComplex(toInexact(o.re), toInexact(o.im))
            is BigRatio   -> o.toBigDecimalInexact()
            is BigInteger -> BigDecimal(o.toString()).setScale(1, Utils.ROUNDING_MODE)
            is BigDecimal -> o.setScale(Math.max(1, o.scale()), Utils.ROUNDING_MODE)
            else          -> (o as Number).toDouble()
        }
    }
}
