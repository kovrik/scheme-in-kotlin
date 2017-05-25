package core.procedures.math.trigonometry

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils
import java.lang.NullPointerException

import java.math.BigDecimal
import java.math.BigInteger

class Sinh : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(Number::class.java)).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "sinh"

    override operator fun invoke(arg: Any?): Number? {
        if (arg == null) throw NullPointerException()
        /* Special cases */
        when {
            Utils.isZero(arg) -> return 0L
            arg is BigDecimal -> return sinh(arg)
            arg is BigInteger -> return sinh(arg)
            arg is BigComplex -> return sinh(arg)
            arg is BigRatio   -> return sinh(arg.toBigDecimal())
            else -> return Math.sinh((arg as Number).toDouble())
        }
    }

    companion object {

        internal fun sinh(bd: BigDecimal): Double {
            val v = bd.toDouble()
            when {
                java.lang.Double.isInfinite(v) || java.lang.Double.isNaN(v) -> return java.lang.Double.NaN
                else -> return Math.sinh(v)
            }
        }

        internal fun sinh(bi: BigInteger): Double {
            val v = bi.toDouble()
            when {
                java.lang.Double.isInfinite(v) || java.lang.Double.isNaN(v) -> return java.lang.Double.NaN
                else -> return Math.sinh(v)
            }
        }

        /* sinh(x + yi) = sinh(x)*cos(y) + cosh(x)*sin(y)*i */
        internal fun sinh(c: BigComplex): Number {
            val x = c.re
            val y = c.im
            val re = sinh(x) * Cos.cos(y)
            val im = Cosh.cosh(x) * Sin.sin(y)
            when {
                java.lang.Double.isInfinite(re) || java.lang.Double.isNaN(re) -> return java.lang.Double.NaN
                java.lang.Double.isInfinite(im) || java.lang.Double.isNaN(im) -> return java.lang.Double.NaN
                else -> return BigComplex(re, im)
            }
        }
    }
}
