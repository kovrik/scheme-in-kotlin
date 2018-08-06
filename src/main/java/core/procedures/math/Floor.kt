package core.procedures.math

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Ratio
import core.scm.Type

import java.math.BigDecimal
import java.math.BigInteger
import java.math.RoundingMode
import kotlin.math.floor

class Floor : AFn<Number?, Number>(name = "floor", isPure = true, arity = Exactly(1),
                                   mandatoryArgsTypes = arrayOf(Type.Real::class.java)) {

    override operator fun invoke(arg: Number?) = when (arg) {
        is Long, is Int, is Short, is Byte, is BigInteger -> arg
        is Double     -> floor(arg)
        is Float      -> floor(arg.toDouble())
        is BigDecimal -> {
            val result = arg.setScale(0, RoundingMode.DOWN)
            when {
                arg.scale() > 0 -> result.setScale(1)
                else -> result
            }
        }
        is Ratio   -> arg.floor()
        else          -> floor(arg!!.toDouble())
    }!!
}
