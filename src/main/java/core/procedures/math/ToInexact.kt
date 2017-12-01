package core.procedures.math

import core.procedures.AFn
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils
import java.math.BigDecimal
import java.math.BigInteger

class ToInexact : AFn<Number?, Number>(name = "exact->inexact", isPure = true, minArgs = 1, maxArgs = 1,
                                       mandatoryArgsTypes = arrayOf(Number::class.java)) {

    override operator fun invoke(arg: Number?): Number = when (arg) {
        is BigComplex -> BigComplex(invoke(arg.re), invoke(arg.im))
        is BigRatio   -> arg.toBigDecimalInexact()
        is BigInteger -> BigDecimal(arg.toString()).setScale(1, Utils.ROUNDING_MODE)
        is BigDecimal -> arg.setScale(maxOf(1, arg.scale()), Utils.ROUNDING_MODE)
        else          -> arg!!.toDouble()
    }
}
