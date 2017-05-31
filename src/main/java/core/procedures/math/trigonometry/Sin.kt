package core.procedures.math.trigonometry

import core.procedures.AFn
import core.procedures.FnArgs
import core.procedures.math.Multiplication
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

class Sin : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(Number::class.java))) {

    override val isPure = true
    override val name = "sin"

    override operator fun invoke(arg: Any?): Number {
        return sin(arg!! as Number)
    }

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
