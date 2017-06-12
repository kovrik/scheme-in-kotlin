package core.procedures.math.trigonometry

import core.procedures.AFn
import core.procedures.math.Multiplication
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

class Sin : AFn<Number?, Number>(name = "sin", isPure = true, minArgs = 1, maxArgs = 1, mandatoryArgsTypes = arrayOf<Class<*>>(Number::class.java)) {

    override operator fun invoke(arg: Number?) = sin(arg!!)

    companion object {

        fun sin(number: Number): Number {
            /* Special cases */
            return when {
                Utils.isZero(number) -> 0L
                number is BigDecimal -> sin(number)
                number is BigInteger -> sin(number)
                number is BigComplex -> sin(number)
                number is BigRatio   -> sin(number.toBigDecimal())
                else -> Math.sin(number.toDouble())
            }
        }

        fun sin(bd: BigDecimal): Double {
            val v = bd.toDouble()
            return when {
                !v.isFinite() -> Double.NaN
                else -> Math.sin(v)
            }
        }

        fun sin(bi: BigInteger): Double {
            val v = bi.toDouble()
            return when {
                !v.isFinite() -> Double.NaN
                else -> Math.sin(v)
            }
        }

        fun sin(c: BigComplex): BigComplex {
            val re = c.re
            val im = c.im
            return BigComplex(Multiplication(Sin.sin(re), Cosh.cosh(im)), Multiplication(Cos.cos(re), Sinh.sinh(im)))
        }
    }
}
