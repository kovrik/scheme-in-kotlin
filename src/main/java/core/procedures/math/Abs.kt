package core.procedures.math

import core.procedures.AFn
import core.scm.BigRatio
import core.scm.Type
import java.math.BigDecimal
import java.math.BigInteger

class Abs : AFn(name = "abs", isPure = true, minArgs = 1, maxArgs = 1, mandatoryArgsTypes = arrayOf<Class<*>>(Type.Real::class.java)) {

    override operator fun invoke(arg: Any?) = abs(arg!! as Number)

    companion object {
        fun abs(number: Number): Number = when (number) {
            is Long       -> Math.abs(number)
            is Int        -> Math.abs(number)
            is Double     -> Math.abs(number)
            is Float      -> Math.abs(number)
            is BigInteger -> number.abs()
            is BigDecimal -> number.abs()
            is BigRatio   -> number.abs()
            else          -> Math.abs(number.toLong())
        }
    }
}
