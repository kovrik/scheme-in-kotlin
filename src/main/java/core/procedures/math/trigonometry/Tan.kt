package core.procedures.math.trigonometry

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils
import java.lang.NullPointerException

import java.math.BigDecimal
import java.math.BigInteger

class Tan : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(Number::class.java)).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "tan"

    override operator fun invoke(arg: Any?): Number {
        if (arg == null) throw NullPointerException()
        /* Special cases */
        when {
            Utils.isZero(arg) -> return 0L
            arg is BigDecimal -> return tan(arg)
            arg is BigInteger -> return tan(arg)
            arg is BigComplex -> return tan(arg)
            arg is BigRatio   -> return tan(arg.toBigDecimal())
            else -> return Math.tan((arg as Number).toDouble())
        }
    }

    private fun tan(bd: BigDecimal): Double {
        val v = bd.toDouble()
        when {
            java.lang.Double.isInfinite(v) || java.lang.Double.isNaN(v) -> return java.lang.Double.NaN
            else -> return Math.tan(v)
        }
    }

    private fun tan(bi: BigInteger): Double {
        val v = bi.toDouble()
        when {
            java.lang.Double.isInfinite(v) || java.lang.Double.isNaN(v) -> return java.lang.Double.NaN
            else -> return Math.tan(v)
        }
    }

    private fun tan(c: BigComplex): BigComplex {
        val sin = Sin.sin(c)
        val cos = Cos.cos(c)
        return sin.divide(cos)
    }
}
