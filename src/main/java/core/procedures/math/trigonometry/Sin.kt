package core.procedures.math.trigonometry

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.procedures.math.Multiplication
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils

import java.lang.NullPointerException
import java.math.BigDecimal
import java.math.BigInteger

class Sin : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(Number::class.java)).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "sin"

    override fun apply1(arg: Any?): Number {
        if (arg == null) throw NullPointerException()
        return sin(arg as Number)
    }

    companion object {

        fun sin(number: Number): Number {
            /* Special cases */
            when {
                Utils.isZero(number) -> return 0L
                number is BigDecimal -> return sin(number)
                number is BigInteger -> return sin(number)
                number is BigComplex -> return sin(number)
                number is BigRatio   -> return sin(number.toBigDecimal())
                else -> return Math.sin(number.toDouble())
            }
        }

        fun sin(bd: BigDecimal): Double {
            val v = bd.toDouble()
            when {
                java.lang.Double.isInfinite(v) || java.lang.Double.isNaN(v) -> return java.lang.Double.NaN
                else -> return Math.sin(v)
            }
        }

        fun sin(bi: BigInteger): Double {
            val v = bi.toDouble()
            when {
                java.lang.Double.isInfinite(v) || java.lang.Double.isNaN(v) -> return java.lang.Double.NaN
                else -> return Math.sin(v)
            }
        }

        fun sin(c: BigComplex): BigComplex {
            val re = c.re
            val im = c.im
            return BigComplex(Multiplication.apply(Sin.sin(re), Cosh.cosh(im)),
                    Multiplication.apply(Cos.cos(re), Sinh.sinh(im)))
        }
    }
}
