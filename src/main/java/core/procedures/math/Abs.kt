package core.procedures.math

import core.procedures.AFn
import core.scm.BigRatio
import core.scm.Type
import java.math.BigDecimal
import java.math.BigInteger

class Abs : AFn<Number?, Number>(name = "abs", isPure = true, minArgs = 1, maxArgs = 1, mandatoryArgsTypes = arrayOf<Class<*>>(Type.Real::class.java)) {

    override operator fun invoke(arg: Number?): Number = when (arg) {
        is Long       -> Math.abs(arg)
        is Int        -> Math.abs(arg)
        is Double     -> Math.abs(arg)
        is Float      -> Math.abs(arg)
        is BigInteger -> arg.abs()
        is BigDecimal -> arg.abs()
        is BigRatio   -> arg.abs()
        else          -> Math.abs(arg!!.toLong())
    }
}
