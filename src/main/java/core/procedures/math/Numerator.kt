package core.procedures.math

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.BigRatio
import core.utils.Utils
import java.lang.NullPointerException

import java.math.BigDecimal

class Numerator : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(BigRatio::class.java)).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "numerator"

    override fun apply1(arg: Any?): Number? {
        if (arg == null) throw NullPointerException()
        return numerator(arg)
    }

    private fun numerator(o: Any): Number {
        val isExact = Utils.isExact(o)
        val exact: Number
        if (isExact) {
            exact = o as Number
        } else {
            exact = ToExact.toExact(o)
        }
        if (exact is BigRatio) {
            if (!isExact) {
                val result = BigDecimal(exact.numerator)
                return result.setScale(1, Utils.ROUNDING_MODE)
            }
            return exact.numerator
        }
        return exact
    }
}
