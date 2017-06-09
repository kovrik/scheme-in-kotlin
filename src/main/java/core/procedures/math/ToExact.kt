package core.procedures.math

import core.procedures.AFn
import core.procedures.functional.ForEach.map.name
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils
import java.math.BigDecimal
import java.math.BigInteger

class ToExact : AFn(name = "inexact->exact", isPure = true, minArgs = 1, maxArgs = 1,
                    mandatoryArgsTypes = arrayOf<Class<*>>(Number::class.java)) {

    override operator fun invoke(arg: Any?) = toExact(arg)

    companion object {

        fun toExact(o: Any?): Number = when {
            Utils.isZero(o) -> o as Number
            o is Float      -> doubleToExact(o.toDouble())
            o is Double     -> doubleToExact(o)
            o is BigDecimal -> bigDecimalToExact(o)
            o is BigComplex -> BigComplex(toExact(o.re), toExact(o.im))
            else            -> o as Number
        }

        private fun doubleToExact(number: Double): Number {
            if (!number.isFinite()) throw ArithmeticException("$name: no exact representation of: Writer.write(number)")
            /* Check if Double is integral */
            val bits = java.lang.Double.doubleToLongBits(number)
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
}
