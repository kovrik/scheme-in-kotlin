package core.procedures.math

import core.procedures.AFn
import core.scm.BigRatio
import core.scm.Type
import java.math.BigDecimal
import java.math.BigInteger

class Sgn : AFn<Number?, Number>(name = "sgn", isPure = true, minArgs = 1, maxArgs = 1,
                                 mandatoryArgsTypes = arrayOf<Class<*>>(Type.Real::class.java)) {

    override operator fun invoke(arg: Number?): Number = when (arg) {
        is Long       -> java.lang.Long.signum(arg)
        is Int        -> java.lang.Integer.signum(arg)
        is Double     -> Math.signum(arg)
        is Float      -> Math.signum(arg)
        is BigInteger -> arg.signum()
        is BigDecimal -> arg.signum()
        is BigRatio   -> arg.signum()
        else          -> java.lang.Long.signum(arg!!.toLong())
    }
}