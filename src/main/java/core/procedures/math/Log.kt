package core.procedures.math

import core.procedures.AFn
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils
import java.math.BigDecimal
import java.math.BigInteger

class Log : AFn(name = "log", isPure = true, minArgs = 1, maxArgs = 1, mandatoryArgsTypes = arrayOf<Class<*>>(Number::class.java)) {

    override operator fun invoke(arg: Any?) = log(arg!! as Number)

    companion object {
        /* If number has 307 digits or less, then can use Math.log(double) */
        private val MAX_DIGITS = 307
        private val VALUE = Math.log(Math.pow(10.0, MAX_DIGITS.toDouble()))

        fun log(number: Number) = when {
            Utils.isZero(number) && Utils.isExact(number) -> throw ArithmeticException("log: undefined for 0")
            Utils.isOne(number)  && Utils.isExact(number) -> 0L
            number is BigComplex                          -> number.log()
            number is BigRatio                            -> logBig(number.toBigDecimal())
            number is BigDecimal                          -> logBig(number)
            number is BigInteger                          -> logBig(BigDecimal(number))
            else                                          -> Math.log(number.toDouble())
        }

        /* Natural logarithm for Big numbers (greater than Double.MAX_VALUE) */
        private fun logBig(number: BigDecimal): Number {
            if (number.toDouble().isFinite()) {
                return Math.log(number.toDouble())
            }
            val digits = integerDigits(number)
            val num = number.movePointLeft(digits - digits.rem(MAX_DIGITS))
            return (digits / MAX_DIGITS) * VALUE + Math.log(num.toDouble())
        }

        /* Return number of digits of a given BigDecimal number */
        private fun integerDigits(n: BigDecimal) = if (n.signum() == 0) 1 else n.precision() - n.scale()
    }
}
