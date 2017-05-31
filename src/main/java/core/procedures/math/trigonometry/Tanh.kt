package core.procedures.math.trigonometry

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils
import java.lang.NullPointerException

import java.math.BigDecimal
import java.math.BigInteger

class Tanh : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(Number::class.java))) {

    override val isPure = true
    override val name = "tanh"

    override operator fun invoke(arg: Any?): Number? {
        if (arg == null) throw NullPointerException()
        /* Special cases */
        return when {
            Utils.isZero(arg) -> 0L
            arg is BigDecimal -> tanh(arg)
            arg is BigInteger -> tanh(arg)
            arg is BigComplex -> tanh(arg)
            arg is BigRatio   -> tanh(arg.toBigDecimal())
            else -> Math.tanh((arg as Number).toDouble())
        }
    }

    private fun tanh(bd: BigDecimal): Double {
        val v = bd.toDouble()
        return when {
            !v.isFinite() -> Double.NaN
            else -> Math.tanh(v)
        }
    }

    private fun tanh(bi: BigInteger): Double {
        val v = bi.toDouble()
        return when {
            !v.isFinite() -> Double.NaN
            else -> Math.tanh(v)
        }
    }

    private fun tanh(c: BigComplex): Number {
        val sinh = Sinh.sinh(c)
        if (sinh is Double && !sinh.isFinite()) {
            return Double.NaN
        }
        val cosh = Cosh.cosh(c)
        if (cosh is Double && !cosh.isFinite()) {
            return Double.NaN
        }
        return (sinh as BigComplex).divide(cosh)
    }
}
