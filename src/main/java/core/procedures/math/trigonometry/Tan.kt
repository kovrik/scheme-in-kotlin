package core.procedures.math.trigonometry

import core.procedures.AFn
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

class Tan : AFn(name = "tan", isPure = true, minArgs = 1, maxArgs = 1, mandatoryArgsTypes = arrayOf<Class<*>>(Number::class.java)) {

    override operator fun invoke(arg: Any?): Number {
        arg!!
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

    private fun tan(c: BigComplex): BigComplex = Sin.sin(c) / Cos.cos(c)
}
