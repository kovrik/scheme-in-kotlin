package core.procedures.math

import core.procedures.AFn
import core.scm.BigRatio
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

open class Quotient : AFn(name = "quotient", isPure = true, minArgs = 2, maxArgs = 2,
                          mandatoryArgsTypes = arrayOf<Class<*>>(Long::class.javaObjectType, Long::class.javaObjectType)) {

    override operator fun invoke(arg1: Any?, arg2: Any?): Number? {
        arg1!!
        arg2!!
        when {
            Utils.isOne(arg2)  -> return Utils.inexactnessTaint(arg1 as Number, arg2 as Number?)
            Utils.isZero(arg2) -> throw ArithmeticException("quotient: undefined for 0")
            else               -> return invoke(arg1 as Number, arg2 as Number)
        }
    }

    private operator fun invoke(first: BigDecimal, second: BigDecimal): Number {
        val scale = Math.max(first.scale(), second.scale())
        if (scale > 0) {
            return first.divide(second, Utils.DEFAULT_CONTEXT).setScale(0, Utils.ROUNDING_MODE).setScale(1, Utils.ROUNDING_MODE)
        } else {
            return first.divideToIntegralValue(second).setScale(scale, Utils.ROUNDING_MODE)
        }
    }

    private operator fun invoke(first: Number, second: Number): Number? {
        val (f, s) = Utils.upcast(first, second)
        return when {
            Utils.isZero(f)                    -> Utils.inexactnessTaint(f, s)
            f is BigDecimal && s is BigDecimal -> invoke(f, s)
            f is BigInteger && s is BigInteger -> f.divide(s)
            f is Double     && s is Double && Utils.isInteger(f) && Utils.isInteger(s) -> (f.toLong() / s.toLong()).toDouble()
            f is Float      && s is Float  && Utils.isInteger(f) && Utils.isInteger(s) -> (f.toLong() / s.toLong()).toDouble()
            f is Double     && s is Double     -> invoke(Utils.toBigDecimal(f), Utils.toBigDecimal(s))
            f is Float      && s is Float      -> invoke(Utils.toBigDecimal(f), Utils.toBigDecimal(s))
            f is BigRatio   && s is BigRatio   -> invoke(Utils.toBigDecimal(f), Utils.toBigDecimal(s))
            else                               -> f.toLong() / s.toLong()
        }
    }
}
