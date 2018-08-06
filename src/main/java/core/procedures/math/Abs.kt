package core.procedures.math

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Ratio
import core.scm.Type
import java.math.BigDecimal
import java.math.BigInteger
import kotlin.math.abs
import kotlin.math.absoluteValue

class Abs : AFn<Number?, Number>(name = "abs", isPure = true, arity = Exactly(1),
                                 mandatoryArgsTypes = arrayOf(Type.Real::class.java)) {

    override operator fun invoke(arg: Number?): Number = when (arg) {
        is Long       -> arg.absoluteValue
        is Int        -> arg.absoluteValue
        is Double     -> arg.absoluteValue
        is Float      -> arg.absoluteValue
        is BigInteger -> arg.abs()
        is BigDecimal -> arg.abs()
        is Ratio   -> arg.abs()
        else          -> abs(arg!!.toLong())
    }
}
