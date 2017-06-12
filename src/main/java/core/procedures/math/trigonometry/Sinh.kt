package core.procedures.math.trigonometry

import core.procedures.AFn
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

class Sinh : AFn<Number?, Number>(name = "sinh", isPure = true, minArgs = 1, maxArgs = 1, mandatoryArgsTypes = arrayOf<Class<*>>(Number::class.java)) {

    override operator fun invoke(arg: Number?): Number {
        arg!!
        /* Special cases */
        return when {
            Utils.isZero(arg) -> 0L
            arg is BigDecimal -> sinh(arg)
            arg is BigInteger -> sinh(arg)
            arg is BigComplex -> sinh(arg)
            arg is BigRatio   -> sinh(arg.toBigDecimal())
            else -> Math.sinh(arg.toDouble())
        }
    }

    companion object {

        internal fun sinh(bd: BigDecimal): Double {
            val v = bd.toDouble()
            return when {
                !v.isFinite()-> Double.NaN
                else -> Math.sinh(v)
            }
        }

        internal fun sinh(bi: BigInteger): Double {
            val v = bi.toDouble()
            return when {
                !v.isFinite() -> Double.NaN
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
                !re.isFinite() -> Double.NaN
                !im.isFinite() -> Double.NaN
                else -> BigComplex(re, im)
            }
        }
    }
}
