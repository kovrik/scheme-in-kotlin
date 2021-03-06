package core.procedures.math

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Complex
import core.scm.Ratio
import core.utils.Utils
import java.math.BigDecimal
import java.math.BigInteger

class ToInexact : AFn<Number?, Number>(name = "exact->inexact", isPure = true, arity = Exactly(1),
                                       mandatoryArgsTypes = arrayOf(Number::class.java)) {

    override operator fun invoke(arg: Number?): Number = when (arg) {
        is Complex    -> Complex(invoke(arg.re), invoke(arg.im))
        is Ratio      -> arg.toBigDecimalInexact()
        is BigInteger -> arg.toBigDecimal().setScale(1, Utils.ROUNDING_MODE)
        is BigDecimal -> arg.setScale(maxOf(1, arg.scale()), Utils.ROUNDING_MODE)
        else          -> arg!!.toDouble()
    }
}
