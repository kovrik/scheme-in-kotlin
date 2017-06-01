package core.procedures.math

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.BigRatio
import core.scm.Type
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger
import java.math.MathContext

class Round : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(Type.Real::class.java))) {

    override val isPure = true
    override val name = "round"
    override operator fun invoke(arg: Any?) = round(arg as Number?)

    private fun round(number: Number?): Number {
        when (number) {
            is Long, is Int, is Short, is Byte, is BigInteger -> return number
            is BigDecimal -> return if (number.scale() == 0) number.round(MathContext.UNLIMITED) else number.round(Utils.DEFAULT_CONTEXT)
            is BigRatio -> return number.round()
            else -> return Math.rint(number!!.toDouble())
        }
    }
}
