package core.procedures.math

import core.procedures.AFn
import core.scm.BigRatio
import core.scm.Type

import java.math.BigDecimal
import java.math.BigInteger
import java.math.RoundingMode

class Round : AFn<Number?, Number>(name = "round", isPure = true, minArgs = 1, maxArgs = 1,
                                   mandatoryArgsTypes = arrayOf<Class<*>>(Type.Real::class.java)) {

    override operator fun invoke(arg: Number?) = round(arg!!)!!

    private fun round(number: Number) = when (number) {
        is Long, is Int, is Short, is Byte, is BigInteger -> number
        is BigDecimal -> number.setScale(0, RoundingMode.HALF_EVEN).setScale(1)
        is BigRatio   -> number.round()
        else          -> Math.rint(number.toDouble())
    }
}
