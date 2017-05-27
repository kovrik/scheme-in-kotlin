package core.procedures.math

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

class ToInexact : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(Number::class.java)).build()) {

    companion object {
        fun toInexact(o: Any?): Number {
            when (o) {
                is BigComplex -> {
                    return BigComplex(toInexact(o.re), toInexact(o.im))
                }
                is BigRatio -> return o.toBigDecimalInexact()
                is BigInteger -> return BigDecimal(o.toString()).setScale(1, Utils.ROUNDING_MODE)
                is BigDecimal -> {
                    val scale = Math.max(1, o.scale())
                    return o.setScale(scale, Utils.ROUNDING_MODE)
                }
                else -> return (o as Number).toDouble()
            }
        }
    }

    override val isPure = true
    override val name = "exact->inexact"

    override operator fun invoke(arg: Any?): Number? {
        return toInexact(arg)
    }
}
