package core.procedures.math.trigonometry

import core.procedures.AFn
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

class Tanh : AFn<Number?, Number>(name = "tanh", isPure = true, minArgs = 1, maxArgs = 1, mandatoryArgsTypes = arrayOf<Class<*>>(Number::class.java)) {

    override operator fun invoke(arg: Number?) = when {
        Utils.isZero(arg) -> 0L
        arg is BigDecimal -> tanh(arg)
        arg is BigInteger -> tanh(arg)
        arg is BigComplex -> tanh(arg)
        arg is BigRatio   -> tanh(arg.toBigDecimal())
        else              -> Math.tanh(arg!!.toDouble())
    }

    private fun tanh(bd: BigDecimal) = bd.toDouble().let {
        when {
            !it.isFinite() -> Double.NaN
            else           -> Math.tanh(it)
        }
    }

    private fun tanh(bi: BigInteger) = bi.toDouble().let {
        when {
            !it.isFinite() -> Double.NaN
            else           -> Math.tanh(it)
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
        return (sinh as BigComplex) / cosh
    }
}
