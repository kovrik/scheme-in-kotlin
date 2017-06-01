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
        val isExact = Utils.isExact(o)
        val exact: Number = if (isExact) (o as Number) else ToExact.toExact(o)
        if (exact is BigRatio) {
            if (!isExact) {
                val result = BigDecimal(exact.denominator)
                return result.setScale(1, Utils.ROUNDING_MODE)
            }
            return exact.denominator
        }
        when (exact) {
            is Long, is Int, is Byte, is Short -> return 1L
            is Double, is Float    -> return 1.0
            is BigInteger          -> return BigInteger.ONE
            is BigDecimal          -> return when {
                exact.scale() == 0 -> BigDecimal.ONE
                else -> BigDecimal.ONE.setScale(1, Utils.ROUNDING_MODE)
            }
            else -> return 1L
        }
    }
}
