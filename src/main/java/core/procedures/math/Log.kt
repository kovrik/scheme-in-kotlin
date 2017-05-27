package core.procedures.math

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

class Log : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(Number::class.java)).build()) {

    override val isPure = true
    override val name = "log"

    override operator fun invoke(arg: Any?): Number? {
        if (arg == null) throw NullPointerException()
        return log(arg as Number)
    }

    companion object {
        /* If number has 307 digits or less, then can use Math.log(double) */
        private val MAX_DIGITS = 307
        private val VALUE = Math.log(Math.pow(10.0, MAX_DIGITS.toDouble()))

        fun log(number: Number): Number {
            if (number is BigComplex) {
                return number.log()
            }
            if (number is Double) {
                return Math.log(number.toDouble())
            }
            val n = Utils.upcast(number)
            if (n is Long) {
                if (n.toLong() == 0L) {
                    throw ArithmeticException("log: undefined for 0")
                }
                if (n.toLong() == 1L) {
                    return 0L
                }
                return Math.log(n.toDouble())
            }
            if (number is BigRatio) {
                if (number == BigRatio.ONE) {
                    return 0L
                }
                return logBig(number.toBigDecimal())
            }
            if (number is BigDecimal) {
                return logBig(number)
            }
            if (number is BigInteger) {
                if (number.signum() == 0) {
                    throw ArithmeticException("log: undefined for 0")
                }
                return logBig(Utils.toBigDecimal(number))
            }
            return Math.log(number.toDouble())
        }

        /* Natural logarithm for Big numbers (greater than Double.MAX_VALUE) */
        private fun logBig(number: BigDecimal): Number {
            var number = number
            if (java.lang.Double.isFinite(number.toDouble())) {
                return Math.log(number.toDouble())
            }
            val digits = integerDigits(number)
            val n = digits / MAX_DIGITS
            number = number.movePointLeft(n * MAX_DIGITS)
            return n * VALUE + Math.log(number.toDouble())
        }

        /* Return number of digits of a given BigDecimal number */
        private fun integerDigits(n: BigDecimal): Int {
            return if (n.signum() == 0) 1 else n.precision() - n.scale()
        }
    }
}
