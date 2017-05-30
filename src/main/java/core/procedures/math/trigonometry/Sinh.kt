package core.procedures.math.trigonometry

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils
import java.lang.NullPointerException

import java.math.BigDecimal
import java.math.BigInteger

class Sinh : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(Number::class.java))) {

    override val isPure = true
    override val name = "sinh"

    override operator fun invoke(arg: Any?): Number? {
        if (arg == null) throw NullPointerException()
        /* Special cases */
        return when {
            Utils.isZero(arg) -> 0L
            arg is BigDecimal -> sinh(arg)
            arg is BigInteger -> sinh(arg)
            arg is BigComplex -> sinh(arg)
            arg is BigRatio   -> sinh(arg.toBigDecimal())
            else -> Math.sinh((arg as Number).toDouble())
        }
    }

    companion object {

        internal fun sinh(bd: BigDecimal): Double {
            val v = bd.toDouble()
            return when {
                java.lang.Double.isInfinite(v) || java.lang.Double.isNaN(v) -> java.lang.Double.NaN
                else -> Math.sinh(v)
            }
        }

        internal fun sinh(bi: BigInteger): Double {
            val v = bi.toDouble()
            return when {
                java.lang.Double.isInfinite(v) || java.lang.Double.isNaN(v) -> java.lang.Double.NaN
                else -> Math.sinh(v)
            }
        }

        /* sinh(x + yi) = sinh(x)*cos(y) + cosh(x)*sin(y)*i */
        internal fun sinh(c: BigComplex): Number {
            val x = c.re
            val y = c.im
            val re = sinh(x) * Cos.cos(y)
            val im = Cosh.cosh(x) * Sin.sin(y)
            return when {
                java.lang.Double.isInfinite(re) || java.lang.Double.isNaN(re) -> java.lang.Double.NaN
                java.lang.Double.isInfinite(im) || java.lang.Double.isNaN(im) -> java.lang.Double.NaN
                else -> BigComplex(re, im)
            }
        }
    }
}
