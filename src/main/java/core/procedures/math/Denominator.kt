package core.procedures.math

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.BigRatio
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

class Denominator : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(BigRatio::class.java))) {

    override val isPure = true
    override val name = "denominator"
    override operator fun invoke(arg: Any?) = denominator(arg!!)

    private fun denominator(o: Any): Number {
        val isIntegerOrRatio = o is BigRatio || Utils.isInteger(o)
        val exact: Number = if (isIntegerOrRatio) (o as Number) else ToExact.toExact(o)
        if (exact is BigRatio) {
            if (!isIntegerOrRatio) {
                return BigDecimal(exact.denominator).setScale(1, Utils.ROUNDING_MODE)
            }
            return exact.denominator
        }
        return when (exact) {
            is Long, is Int, is Byte, is Short -> 1L
            is Double, is Float    -> 1.0
            is BigInteger          -> BigInteger.ONE
            is BigDecimal          -> when {
                exact.scale() == 0 -> BigDecimal.ONE
                else               -> BigDecimal.ONE.setScale(1, Utils.ROUNDING_MODE)
            }
            else -> 1L
        }
    }
}
