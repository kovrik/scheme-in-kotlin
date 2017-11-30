package core.procedures.math

import core.procedures.AFn
import core.scm.BigRatio
import core.scm.Type
import java.math.BigDecimal
import java.math.BigInteger
import kotlin.math.sign

class Sgn : AFn<Number?, Number>(name = "sgn", isPure = true, minArgs = 1, maxArgs = 1,
                                 mandatoryArgsTypes = arrayOf<Class<*>>(Type.Real::class.java)) {

    override operator fun invoke(arg: Number?): Number = when (arg) {
        is Long       -> arg.sign
        is Int        -> arg.sign
        is Double     -> arg.sign
        is Float      -> arg.sign
        is BigInteger -> arg.signum()
        is BigDecimal -> arg.signum()
        is BigRatio   -> arg.signum()
        else          -> arg!!.toLong().sign
    }
}