package core.procedures.math.trigonometry

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

class Cosh : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(Number::class.java))) {

    override val isPure = true
    override val name = "cosh"

    override operator fun invoke(arg: Any?): Number? {
        arg!!
        /* Special cases */
        return when {
            Utils.isZero(arg) -> 1L
            arg is BigDecimal -> cosh(arg)
            arg is BigInteger -> cosh(arg)
            arg is BigComplex -> cosh(arg)
            arg is BigRatio   -> cosh(arg.toBigDecimal())
            else -> Math.cosh((arg as Number).toDouble())
        }
    }

    companion object {

        internal fun cosh(bd: BigDecimal): Double {
            val v = bd.toDouble()
            return when {
                !v.isFinite() -> Double.NaN
                else -> Math.cosh(v)
            }
        }

        internal fun cosh(bi: BigInteger): Double {
            val v = bi.toDouble()
            return when {
                !v.isFinite() -> Double.NaN
                else -> Math.cosh(v)
            }
        }

        /* cosh(x + yi) = cosh(x)*cos(y) + sinh(x)*sin(y)*i */
        internal fun cosh(c: BigComplex): Number {
            val x = c.re
            val y = c.im
            val re = Cosh.cosh(x) * Cos.cos(y)
            val im = Sinh.sinh(x) * Sin.sin(y)
            return when {
                !re.isFinite() -> Double.NaN
                !im.isFinite() -> Double.NaN
                else -> BigComplex(re, im)
            }
        }
    }
}
