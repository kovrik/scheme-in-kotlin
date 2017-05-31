package core.procedures.math

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.BigRatio
import java.math.BigDecimal

class Exp : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(Number::class.java))) {

    override val isPure = true
    override val name = "exp"

    override operator fun invoke(arg: Any?): Number? {
        return exp(arg as Number?)
    }

    companion object {

        val E = BigDecimal("2.71828182845904523536028747135266249775724709369995")

        fun exp(number: Number?): Number? {
            number!!
            if (number is Double) {
                if (number == Double.NEGATIVE_INFINITY) {
                    return 0L
                }
                if (!number.isFinite()) {
                    return number
                }
                return Math.exp(number.toDouble())
            }
            if (number is Float) {
                if (number == Float.NEGATIVE_INFINITY) {
                    return 0L
                }
                if (!number.isFinite()) {
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
