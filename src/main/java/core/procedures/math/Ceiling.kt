package core.procedures.math

import core.procedures.AFn
import core.scm.BigRatio
import core.scm.Type

import java.math.BigDecimal
import java.math.BigInteger

class Ceiling : AFn<Number?, Number>(name = "ceiling", isPure = true, minArgs = 1, maxArgs = 1, mandatoryArgsTypes = arrayOf<Class<*>>(Type.Real::class.java)) {

    override operator fun invoke(arg: Number?): Number {
        arg!!
        return when (arg) {
            is Long, is Int, is Short, is Byte, is BigInteger -> arg
            is Double     -> Math.ceil((arg as Double?)!!)
            is Float      -> Math.ceil((arg as Float?)!!.toDouble())
            is BigDecimal -> arg.setScale(0, BigDecimal.ROUND_UP)
            is BigRatio   -> arg.ceiling()
            else          -> Math.ceil(arg.toDouble())
        }
    }
}
