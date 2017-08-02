package core.procedures.math.trigonometry

import core.procedures.AFn
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

class Sinh : AFn<Number?, Number>(name = "sinh", isPure = true, minArgs = 1, maxArgs = 1, mandatoryArgsTypes = arrayOf<Class<*>>(Number::class.java)) {

    override operator fun invoke(arg: Number?) = when {
        Utils.isZero(arg) -> 0L
        arg is BigDecimal -> sinh(arg)
        arg is BigInteger -> sinh(arg)
        arg is BigComplex -> sinh(arg)
        arg is BigRatio   -> sinh(arg.toBigDecimal())
        else              -> Math.sinh(arg!!.toDouble())
    }

    companion object {

        internal fun sinh(bd: BigDecimal) = bd.toDouble().let {
            when {
                !it.isFinite()-> Double.NaN
                else          -> Math.sinh(it)
            }
        }

        internal fun sinh(bi: BigInteger) = bi.toDouble().let {
            when {
                !it.isFinite() -> Double.NaN
                else           -> Math.sinh(it)
            }
        }

        /* sinh(x + yi) = sinh(x)*cos(y) + cosh(x)*sin(y)*i */
        internal fun sinh(c: BigComplex): Number {
            val re = sinh(c.re) * Cos.cos(c.im)
            val im = Cosh.cosh(c.re) * Sin.sin(c.im)
            return when {
                !re.isFinite() -> Double.NaN
                !im.isFinite() -> Double.NaN
                else -> BigComplex(re, im)
            }
        }
    }
}
