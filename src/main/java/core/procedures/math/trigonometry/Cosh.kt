package core.procedures.math.trigonometry

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils
import java.lang.NullPointerException

import java.math.BigDecimal
import java.math.BigInteger

class Cosh : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(Number::class.java)).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "cosh"

    override fun apply1(arg: Any?): Number? {
        if (arg == null) throw NullPointerException()
        /* Special cases */
        when {
            Utils.isZero(arg) -> return 1L
            arg is BigDecimal -> return cosh(arg)
            arg is BigInteger -> return cosh(arg)
            arg is BigComplex -> return cosh(arg)
            arg is BigRatio   -> return cosh(arg.toBigDecimal())
            else -> return Math.cosh((arg as Number).toDouble())
        }
    }

    companion object {

        internal fun cosh(bd: BigDecimal): Double {
            val v = bd.toDouble()
            when {
                java.lang.Double.isInfinite(v) || java.lang.Double.isNaN(v) -> return java.lang.Double.NaN
                else -> return Math.cosh(v)
            }
        }

        internal fun cosh(bi: BigInteger): Double {
            val v = bi.toDouble()
            when {
                java.lang.Double.isInfinite(v) || java.lang.Double.isNaN(v) -> return java.lang.Double.NaN
                else -> return Math.cosh(v)
            }
        }

        /* cosh(x + yi) = cosh(x)*cos(y) + sinh(x)*sin(y)*i */
        internal fun cosh(c: BigComplex): Number {
            val x = c.re
            val y = c.im
            val re = Cosh.cosh(x) * Cos.cos(y)
            val im = Sinh.sinh(x) * Sin.sin(y)
            when {
                java.lang.Double.isInfinite(re) || java.lang.Double.isNaN(re) -> return java.lang.Double.NaN
                java.lang.Double.isInfinite(im) || java.lang.Double.isNaN(im) -> return java.lang.Double.NaN
                else -> return BigComplex(re, im)
            }
        }
    }
}
