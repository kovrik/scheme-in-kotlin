package core.procedures.math

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.BigRatio
import java.lang.NullPointerException
import java.math.BigDecimal

class Exp : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(Number::class.java)).build()) {

    override val isPure = true
    override val name = "exp"

    override operator fun invoke(arg: Any?): Number? {
        return exp(arg as Number?)
    }

    companion object {

        val E = BigDecimal("2.71828182845904523536028747135266249775724709369995")

        fun exp(number: Number?): Number? {
            if (number == null) throw NullPointerException()
            if (number is Double) {
                if (number == java.lang.Double.NEGATIVE_INFINITY) {
                    return 0L
                }
                if (java.lang.Double.isNaN(number) || java.lang.Double.isInfinite(number)) {
                    return number
                }
                return Math.exp(number.toDouble())
            }
            if (number is Float) {
                if (number == java.lang.Float.NEGATIVE_INFINITY) {
                    return 0L
                }
                if (java.lang.Float.isNaN(number) || java.lang.Float.isInfinite(number)) {
                    return number
                }
                return Math.exp(number.toDouble())
            }
            if (number is Long || number is Byte || number is Short || number is Int) {
                if (number.toLong() == 0L) {
                    return 1L
                }
                return Math.exp(number.toDouble())
            }
            if (number is BigRatio) {
                /* Special cases */
                when {
                    number.isZero -> return 1L
                    number.isOne  -> return Math.exp(1.0)
                }
            }
            return Expt.expt(E, number)
        }
    }
}
