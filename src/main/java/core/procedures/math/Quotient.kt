package core.procedures.math

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Ratio
import core.utils.Utils
import core.utils.Utils.taint

import java.math.BigDecimal
import java.math.BigInteger

open class Quotient : AFn<Any?, Number?>(name = "quotient", isPure = true, arity = Exactly(2),
                          mandatoryArgsTypes = arrayOf(Long::class.javaObjectType, Long::class.javaObjectType)) {

    override operator fun invoke(arg1: Any?, arg2: Any?) = when {
        Utils.isOne(arg2)  -> arg2!! as Number taint arg1!! as Number
        Utils.isZero(arg2) -> throw ArithmeticException("$name: undefined for 0")
        else               -> invoke(arg1!! as Number, arg2!! as Number)
    }

    private operator fun invoke(f: BigDecimal, s: BigDecimal) = maxOf(f.scale(), s.scale()).let {
        when {
            it > 0 -> f.divide(s, Utils.DEFAULT_CONTEXT).setScale(0, Utils.ROUNDING_MODE).setScale(1, Utils.ROUNDING_MODE)
            else   -> f.divideToIntegralValue(s).setScale(it, Utils.ROUNDING_MODE)
        }
    }

    private operator fun invoke(first: Number, second: Number): Number? {
        val (f, s) = Utils.upcast(first, second)
        return when {
            Utils.isZero(f)                    -> s taint f
            f is BigDecimal && s is BigDecimal -> invoke(f, s)
            f is BigInteger && s is BigInteger -> f.divide(s)
            f is Double     && s is Double && Utils.isInteger(f) && Utils.isInteger(s) -> (f.toLong() / s.toLong()).toDouble()
            f is Float      && s is Float  && Utils.isInteger(f) && Utils.isInteger(s) -> (f.toLong() / s.toLong()).toDouble()
            f is Double     && s is Double     -> invoke(Utils.toBigDecimal(f), Utils.toBigDecimal(s))
            f is Float      && s is Float      -> invoke(Utils.toBigDecimal(f), Utils.toBigDecimal(s))
            f is Ratio      && s is Ratio      -> invoke(Utils.toBigDecimal(f), Utils.toBigDecimal(s))
            else                               -> f.toLong() / s.toLong()
        }
    }
}
