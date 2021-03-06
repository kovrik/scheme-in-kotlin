package core.procedures.math

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Ratio
import core.scm.Type
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

class Denominator : AFn<Number?, Number>(name = "denominator", isPure = true, arity = Exactly(1),
                                         mandatoryArgsTypes = arrayOf(Type.Rational::class.java)) {

    private val toExact = ToExact()

    override operator fun invoke(arg: Number?): Number {
        val isIntegerOrRatio = arg is Ratio || Utils.isInteger(arg)
        val exact = if (isIntegerOrRatio) (arg as Number) else toExact(arg as Number)
        if (exact is Ratio) {
            if (!isIntegerOrRatio) {
                return exact.denominator.toBigDecimal().setScale(1, Utils.ROUNDING_MODE)
            }
            return exact.denominator
        }
        return when (exact) {
            is Long, is Int, is Byte, is Short -> 1L
            is Double, is Float    -> 1.0
            is BigInteger          -> BigInteger.ONE
            is BigDecimal          -> when (exact.scale()) {
                0    -> BigDecimal.ONE
                else -> BigDecimal.ONE.setScale(1, Utils.ROUNDING_MODE)
            }
            else -> 1L
        }
    }
}
