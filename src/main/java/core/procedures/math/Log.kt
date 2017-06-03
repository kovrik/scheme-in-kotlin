package core.procedures.math

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils
import java.math.BigDecimal
import java.math.BigInteger

class Log : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(Number::class.java))) {

    override val isPure = true
    override val name = "log"
    override operator fun invoke(arg: Any?) = log(arg!! as Number)

    companion object {
        /* If number has 307 digits or less, then can use Math.log(double) */
        private val MAX_DIGITS = 307
        private val VALUE = Math.log(Math.pow(10.0, MAX_DIGITS.toDouble()))

        fun log(number: Number): Number {
            return when {
                Utils.isZero(number) && Utils.isExact(number) -> throw ArithmeticException("log: undefined for 0")
                Utils.isOne(number)  && Utils.isExact(number) -> 0L
                number is BigComplex                          -> number.log()
                number is BigRatio                            -> logBig(number.toBigDecimal())
                number is BigDecimal                          -> logBig(number)
                number is BigInteger                          -> logBig(Utils.toBigDecimal(number))
                else                                          -> Math.log(number.toDouble())
            }
        }

        /* Natural logarithm for Big numbers (greater than Double.MAX_VALUE) */
        private fun logBig(number: BigDecimal): Number {
            if (number.toDouble().isFinite()) {
                return Math.log(number.toDouble())
            }
            val digits = integerDigits(number)
            val n = digits / MAX_DIGITS
            val num = number.movePointLeft(n * MAX_DIGITS)
            return n * VALUE + Math.log(num.toDouble())
        }

        /* Return number of digits of a given BigDecimal number */
        private fun integerDigits(n: BigDecimal): Int {
            return if (n.signum() == 0) 1 else n.precision() - n.scale()
        }
    }
}
