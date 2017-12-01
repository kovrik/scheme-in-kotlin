package core.procedures.math

import core.procedures.AFn
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils
import core.writer.Writer
import java.math.BigDecimal
import java.math.BigInteger

class ToExact : AFn<Number?, Number>(name = "inexact->exact", isPure = true, minArgs = 1, maxArgs = 1,
                                     mandatoryArgsTypes = arrayOf(Number::class.java)) {

    override operator fun invoke(arg: Number?): Number = when {
        Utils.isZero(arg) -> arg as Number
        arg is Float      -> doubleToExact(arg.toDouble())
        arg is Double     -> doubleToExact(arg)
        arg is BigDecimal -> bigDecimalToExact(arg)
        arg is BigComplex -> BigComplex(invoke(arg.re), invoke(arg.im))
        else              -> arg as Number
    }

    private fun doubleToExact(number: Double): Number {
        if (!number.isFinite()) throw ArithmeticException("inexact->exact: no exact representation of: ${Writer.write(number)}")
        /* Check if Double is integral */
        try {
            val bits = number.toBits()
            val sign = bits.ushr(63)
            val exponent = (bits.ushr(52) xor (sign shl 11)) - 1023
            val fraction = bits shl 12
            var a = 1L
            var b = 1L
            for (i in 63 downTo 12) {
                a = Math.addExact(Math.multiplyExact(a, 2), (fraction.ushr(i) and 1))
                b = Math.multiplyExact(b, 2)
            }
            if (exponent > 0) {
                a = Math.multiplyExact(a, (1 shl exponent.toInt()).toLong())
            } else {
                b = Math.multiplyExact(b, (1 shl -exponent.toInt()).toLong())
            }
            if (sign == 1L) {
                a = Math.multiplyExact(a, -1)
            }
            return BigRatio.valueOf(BigInteger.valueOf(a), BigInteger.valueOf(b))
        } catch (e: ArithmeticException) {
            return bigDecimalToExact(BigDecimal(number.toString()))
        }
    }

    // FIXME Use the same algorithm as for Doubles!
    private fun bigDecimalToExact(number: BigDecimal) = when {
        number.scale() > 0 -> BigRatio.valueOf(number.unscaledValue(), BigInteger.TEN.pow(number.scale()))
        else -> BigRatio.valueOf(number.unscaledValue().multiply(BigInteger.TEN.pow(-number.scale())), BigInteger.ONE)
    }
}
