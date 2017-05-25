package core.procedures.math

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils
import core.writer.Writer

import java.math.BigDecimal
import java.math.BigInteger

class ToExact : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(Number::class.java)).build()) {

    companion object {

        private val NAME = "inexact->exact"

        fun toExact(o: Any?): Number {
            /* Special cases */
            if (Utils.isZero(o)) {
                when (o) {
                    is Double -> return 0.0
                    is Float -> return 0f
                    is BigDecimal -> return BigDecimal.ZERO
                }
            }
            if (o is BigComplex) {
                return BigComplex(toExact(o.re), toExact(o.im))
            }
            if (o is Float) {
                val f = o
                if (java.lang.Float.isInfinite(f) || java.lang.Float.isNaN(f)) {
                    throw ArithmeticException(NAME + ": no exact representation of: " + Writer.write(f))
                }
                /* Check if Double is integral */
                if (f == Math.floor(f.toDouble())) {
                    return f
                }
                return doubleToExact(f.toDouble())
            }
            if (o is Double) {
                val d = o
                if (java.lang.Double.isInfinite(d) || java.lang.Double.isNaN(d)) {
                    throw ArithmeticException(NAME + ": no exact representation of: " + Writer.write(d))
                }
                /* Check if Double is integral */
                if (d == Math.floor(d)) {
                    return d
                }
                return doubleToExact(d)
            }
            if (o is BigDecimal) {
                return bigDecimalToExact(o)
            }
            return o as Number
        }

        private fun doubleToExact(number: Double?): Number {
            val bits = java.lang.Double.doubleToLongBits(number!!)
            val sign = bits.ushr(63)
            val exponent = (bits.ushr(52) xor (sign shl 11)) - 1023
            val fraction = bits shl 12
            var a = 1L
            var b = 1L
            for (i in 63 downTo 12) {
                a = a * 2 + (fraction.ushr(i) and 1)
                b *= 2
            }
            if (exponent > 0) {
                a *= (1 shl exponent.toInt()).toLong()
            } else {
                b *= (1 shl -exponent.toInt()).toLong()
            }
            if (sign == 1L) {
                a *= -1
            }
            return BigRatio.valueOf(BigInteger.valueOf(a), BigInteger.valueOf(b))
        }

        private fun bigDecimalToExact(number: BigDecimal): BigRatio {
            val scale = number.scale()
            return if (scale > 0)
                BigRatio.valueOf(number.unscaledValue(), BigInteger.TEN.pow(scale))
            else
                BigRatio.valueOf(number.unscaledValue().multiply(BigInteger.TEN.pow(-scale)), BigInteger.ONE)
        }
    }

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = NAME

    override operator fun invoke(arg: Any?): Number? {
        return toExact(arg)
    }
}
