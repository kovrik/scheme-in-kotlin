package core.procedures.math.trigonometry

import core.procedures.AFn
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

class Cosh : AFn<Number?, Number>(name = "cosh", isPure = true, minArgs = 1, maxArgs = 1, mandatoryArgsTypes = arrayOf<Class<*>>(Number::class.java)) {

    override operator fun invoke(arg: Number?) = when {
        Utils.isZero(arg) -> 1L
        arg is BigDecimal -> cosh(arg)
        arg is BigInteger -> cosh(arg)
        arg is BigComplex -> cosh(arg)
        arg is BigRatio   -> cosh(arg.toBigDecimal())
        else              -> Math.cosh(arg!!.toDouble())
    }

    companion object {

        internal fun cosh(bd: BigDecimal) = bd.toDouble().let {
            when {
                !it.isFinite() -> Double.NaN
                else           -> Math.cosh(it)
            }
        }

        internal fun cosh(bi: BigInteger) = bi.toDouble().let {
            when {
                !it.isFinite() -> Double.NaN
                else           -> Math.cosh(it)
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
