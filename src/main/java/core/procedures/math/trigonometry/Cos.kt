package core.procedures.math.trigonometry

import core.procedures.AFn
import core.procedures.FnArgs
import core.procedures.math.Multiplication
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils

import java.lang.NullPointerException
import java.math.BigDecimal
import java.math.BigInteger

class Cos : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(Number::class.java))) {

    override val isPure = true
    override val name = "cos"

    override operator fun invoke(arg: Any?): Number {
        if (arg == null) throw NullPointerException()
        return cos(arg as Number)
    }

    companion object {

        fun cos(number: Number): Number {
            /* Special cases */
            when {
                Utils.isZero(number) -> return 1L
                number is BigDecimal -> return cos(number)
                number is BigInteger -> return cos(number)
                number is BigComplex -> return Cos.cos(number)
                number is BigRatio   -> return cos(number.toBigDecimal())
                else -> return Math.cos(number.toDouble())
            }
        }

        fun cos(bd: BigDecimal): Double {
            val v = bd.toDouble()
            when {
                java.lang.Double.isInfinite(v) || java.lang.Double.isNaN(v) -> return java.lang.Double.NaN
                else -> return Math.cos(v)
            }
        }

        fun cos(bi: BigInteger): Double {
            val v = bi.toDouble()
            when {
                java.lang.Double.isInfinite(v) || java.lang.Double.isNaN(v) -> return java.lang.Double.NaN
                else -> return Math.cos(v)
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
