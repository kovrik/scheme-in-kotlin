package core.procedures.math.trigonometry

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils
import java.lang.NullPointerException

import java.math.BigDecimal
import java.math.BigInteger

class Tan : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(Number::class.java))) {

    override val isPure = true
    override val name = "tan"

    override operator fun invoke(arg: Any?): Number {
        if (arg == null) throw NullPointerException()
        /* Special cases */
        return when {
            Utils.isZero(arg) -> 0L
            arg is BigDecimal -> tan(arg)
            arg is BigInteger -> tan(arg)
            arg is BigComplex -> tan(arg)
            arg is BigRatio   -> tan(arg.toBigDecimal())
            else -> Math.tan((arg as Number).toDouble())
        }
    }

    private fun tan(bd: BigDecimal): Double {
        val v = bd.toDouble()
        return when {
            !v.isFinite() -> Double.NaN
            else -> Math.tan(v)
        }
    }

    private fun tan(bi: BigInteger): Double {
        val v = bi.toDouble()
        return when {
            !v.isFinite() -> Double.NaN
            else -> Math.tan(v)
        }
    }

    private fun tan(c: BigComplex): BigComplex {
        val sin = Sin.sin(c)
        val cos = Cos.cos(c)
        return sin.divide(cos)
    }
}
