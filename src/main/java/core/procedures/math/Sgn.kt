package core.procedures.math

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Ratio
import core.scm.Type
import java.math.BigDecimal
import java.math.BigInteger
import kotlin.math.sign

class Sgn : AFn<Number?, Number>(name = "sgn", isPure = true, arity = Exactly(1),
                                 mandatoryArgsTypes = arrayOf(Type.Real::class.java)) {

    override operator fun invoke(arg: Number?): Number = when (arg) {
        is Long       -> arg.sign
        is Int        -> arg.sign
        is Double     -> arg.sign
        is Float      -> arg.sign
        is BigInteger -> arg.signum()
        is BigDecimal -> arg.signum()
        is Ratio   -> arg.signum()
        else          -> arg!!.toLong().sign
    }
}