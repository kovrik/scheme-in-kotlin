package core.procedures.math.trigonometry

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils
import java.lang.NullPointerException

import java.math.BigDecimal
import java.math.BigInteger

class Tanh : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(Number::class.java)).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "tanh"

    override fun apply1(arg: Any?): Number? {
        if (arg == null) throw NullPointerException()
        /* Special cases */
        when {
            Utils.isZero(arg) -> return 0L
            arg is BigDecimal -> return tanh(arg)
            arg is BigInteger -> return tanh(arg)
            arg is BigComplex -> return tanh(arg)
            arg is BigRatio   -> return tanh(arg.toBigDecimal())
            else -> return Math.tanh((arg as Number).toDouble())
        }
    }

    private fun tanh(bd: BigDecimal): Double {
        val v = bd.toDouble()
        when {
            java.lang.Double.isInfinite(v) || java.lang.Double.isNaN(v) -> return java.lang.Double.NaN
            else -> return Math.tanh(v)
        }
    }

    private fun tanh(bi: BigInteger): Double {
        val v = bi.toDouble()
        when {
            java.lang.Double.isInfinite(v) || java.lang.Double.isNaN(v) -> return java.lang.Double.NaN
            else -> return Math.tanh(v)
        }
    }

    private fun tanh(c: BigComplex): Number {
        val sinh = Sinh.sinh(c)
        if (sinh is Double && (java.lang.Double.isInfinite(sinh) || java.lang.Double.isNaN(sinh))) {
            return java.lang.Double.NaN
        }
        val cosh = Cosh.cosh(c)
        if (cosh is Double && (java.lang.Double.isInfinite(cosh) || java.lang.Double.isNaN(cosh))) {
            return java.lang.Double.NaN
        }
        return (sinh as BigComplex).divide(cosh)
    }
}
