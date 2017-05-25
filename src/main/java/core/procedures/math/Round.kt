package core.procedures.math

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.BigRatio
import core.scm.Type
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger
import java.math.MathContext

class Round : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(Type.Real::class.java)).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "round"

    override operator fun invoke(arg: Any?): Number {
        return round(arg as Number?)
    }

    private fun round(number: Number?): Number {
        if (number is Long || number is Int || number is Short || number is Byte || number is BigInteger) {
            return number
        } else if (number is BigDecimal) {
            val bd = number
            return if (bd.scale() == 0) bd.round(MathContext.UNLIMITED) else bd.round(Utils.DEFAULT_CONTEXT)
        } else if (number is BigRatio) {
            return number.round()
        }
        return Math.rint(number!!.toDouble())
    }
}
