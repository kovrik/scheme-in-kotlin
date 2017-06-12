package core.procedures.math.trigonometry

import core.procedures.AFn
import core.procedures.math.Multiplication
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

class Cos : AFn<Number?, Number>(name = "cos", isPure = true, minArgs = 1, maxArgs = 1, mandatoryArgsTypes = arrayOf<Class<*>>(Number::class.java)) {

    override operator fun invoke(arg: Number?) = cos(arg!!)

    companion object {

        fun cos(number: Number): Number {
            /* Special cases */
            return when {
                Utils.isZero(number) -> 1L
                number is BigDecimal -> cos(number)
                number is BigInteger -> cos(number)
                number is BigComplex -> Cos.cos(number)
                number is BigRatio   -> cos(number.toBigDecimal())
                else                 -> Math.cos(number.toDouble())
            }
        }

        fun cos(bd: BigDecimal): Double {
            val v = bd.toDouble()
            return when {
                !v.isFinite() -> Double.NaN
                else -> Math.cos(v)
            }
        }

        fun cos(bi: BigInteger): Double {
            val v = bi.toDouble()
            return when {
                !v.isFinite() -> Double.NaN
                else -> Math.cos(v)
            }
        }

        fun cos(c: BigComplex): BigComplex {
            val re = c.re
            val im = c.im
            return BigComplex(Multiplication(Cos.cos(re), Cosh.cosh(im)),
                    Multiplication(-1.0, Multiplication(Sin.sin(re), Sinh.sinh(im))))
        }
    }
}
