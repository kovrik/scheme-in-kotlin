package core.procedures.math

import core.procedures.AFn
import core.scm.BigRatio
import core.scm.Type
import core.utils.Utils

import java.math.BigDecimal

class Numerator : AFn<Number?, Number>(name = "numerator", isPure = true, minArgs = 1, maxArgs = 1,
                      mandatoryArgsTypes = arrayOf<Class<*>>(Type.Rational::class.java)) {

    override operator fun invoke(arg: Number?) = numerator(arg!!)

    private fun numerator(o: Any): Number {
        val isIntegerOrRatio = o is BigRatio || Utils.isInteger(o)
        val exact: Number = if (isIntegerOrRatio) (o as Number) else ToExact.toExact(o)
        if (exact is BigRatio) {
            if (!isIntegerOrRatio) {
                return BigDecimal(exact.numerator).setScale(1, Utils.ROUNDING_MODE)
            }
            return exact.numerator
        }
        return exact
    }
}
