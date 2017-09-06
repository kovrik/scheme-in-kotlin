package core.procedures.math

import core.procedures.AFn
import core.scm.BigRatio
import core.scm.Type
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger
import java.math.MathContext

class Round : AFn<Number?, Number>(name = "round", isPure = true, minArgs = 1, maxArgs = 1, mandatoryArgsTypes = arrayOf<Class<*>>(Type.Real::class.java)) {

    override operator fun invoke(arg: Number?) = round(arg!!)!!

    private fun round(number: Number) = when (number) {
        is Long, is Int, is Short, is Byte, is BigInteger -> number
        is BigDecimal -> if (number.scale() == 0) number.round(MathContext.UNLIMITED) else number.round(Utils.DEFAULT_CONTEXT)
        is BigRatio   -> number.round()
        else          -> Math.rint(number.toDouble())
    }
}
