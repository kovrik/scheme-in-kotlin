package core.procedures.math.trigonometry

import core.procedures.AFn
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

class Tan : AFn<Number?, Number>(name = "tan", isPure = true, minArgs = 1, maxArgs = 1, mandatoryArgsTypes = arrayOf<Class<*>>(Number::class.java)) {

    override operator fun invoke(arg: Number?) = when {
        Utils.isZero(arg) -> 0L
        arg is BigDecimal -> tan(arg)
        arg is BigInteger -> tan(arg)
        arg is BigComplex -> tan(arg)
        arg is BigRatio   -> tan(arg.toBigDecimal())
        else              -> Math.tan(arg!!.toDouble())
    }

    private fun tan(bd: BigDecimal) = bd.toDouble().let {
        when {
            !it.isFinite() -> Double.NaN
            else           -> Math.tan(it)
        }
    }

    private fun tan(bi: BigInteger) = bi.toDouble().let {
        when {
            !it.isFinite() -> Double.NaN
            else           -> Math.tan(it)
        }
    }

    private fun tan(c: BigComplex): BigComplex = Sin.sin(c) / Cos.cos(c)
}
