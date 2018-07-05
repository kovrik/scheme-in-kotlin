package core.procedures.math

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.BigRatio
import core.utils.Utils
import java.math.BigDecimal
import java.math.BigInteger

class Remainder : AFn<Number?, Number>(name = "remainder", isPure = true, arity = Exactly(2),
                                       mandatoryArgsTypes = arrayOf(Long::class.javaObjectType,
                                                                    Long::class.javaObjectType)) {

    override operator fun invoke(arg1: Number?, arg2: Number?): Number {
        val (f, s) = Utils.upcast(arg1!!, arg2!!)
        return when {
            Utils.isZero(s) -> throw ArithmeticException("$name: undefined for 0")
            Utils.isZero(f) -> Utils.inexactnessTaint(f, s)
            f is BigRatio && s is BigRatio -> f % s
            f is BigDecimal && s is BigDecimal -> f % s
            f is BigInteger && s is BigInteger -> f % s
            Utils.isExact(f) && Utils.isExact(s) -> f.toLong() % s.toLong()
            else -> (f.toDouble() % s.toDouble()).let {
                /* Don't want negative zero */
                when (it) {
                    -0.0 -> 0.0
                    else -> it
                }
            }
        }
    }
}
