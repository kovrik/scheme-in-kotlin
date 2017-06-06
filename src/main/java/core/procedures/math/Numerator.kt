package core.procedures.math

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.BigRatio
import core.utils.Utils

import java.math.BigDecimal

class Numerator : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(BigRatio::class.java))) {

    override val isPure = true
    override val name = "numerator"
    override operator fun invoke(arg: Any?) = numerator(arg!!)

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
