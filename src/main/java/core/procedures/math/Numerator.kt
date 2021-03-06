package core.procedures.math

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Ratio
import core.scm.Type
import core.utils.Utils

class Numerator : AFn<Number?, Number>(name = "numerator", isPure = true, arity = Exactly(1),
                                       mandatoryArgsTypes = arrayOf(Type.Rational::class.java)) {

    private val toExact = ToExact()

    override operator fun invoke(arg: Number?) = numerator(arg!!)

    private fun numerator(o: Any): Number {
        val isIntegerOrRatio = o is Ratio || Utils.isInteger(o)
        val exact: Number = if (isIntegerOrRatio) (o as Number) else toExact(o as Number)
        if (exact is Ratio) {
            if (!isIntegerOrRatio) {
                return exact.numerator.toBigDecimal().setScale(1, Utils.ROUNDING_MODE)
            }
            return exact.numerator
        }
        return exact
    }
}
