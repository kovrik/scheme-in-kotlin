package core.procedures.math

import core.procedures.AFn
import core.scm.BigRatio
import core.utils.Utils
import java.math.BigDecimal
import java.math.BigInteger

class Remainder : AFn<Number?, Number>(name = "remainder", isPure = true, minArgs = 2, maxArgs = 2,
                      mandatoryArgsTypes = arrayOf<Class<*>>(Long::class.javaObjectType, Long::class.javaObjectType)) {

    override operator fun invoke(arg1: Number?, arg2: Number?) = remainder(arg1!!, arg2!!)

    companion object {
        fun remainder(first: Number, second: Number): Number {
            val (f, s) = Utils.upcast(first, second)
            when {
                Utils.isZero(s)                      -> throw ArithmeticException("remainder: undefined for 0")
                Utils.isZero(f)                      -> return Utils.inexactnessTaint(f, s)
                f is BigRatio    && s is BigRatio    -> return Utils.toBigDecimal(f).remainder(Utils.toBigDecimal(s))
                f is BigDecimal  && s is BigDecimal  -> return Utils.toBigDecimal(f).remainder(Utils.toBigDecimal(s))
                f is BigInteger  && s is BigInteger  -> return Utils.toBigInteger(f).remainder(Utils.toBigInteger(s))
                Utils.isExact(f) && Utils.isExact(s) -> return f.toLong() % s.toLong()
                else -> {
                    val result = f.toDouble() % s.toDouble()
                    return when (result) {
                        /* Don't want negative zero */
                        -0.0 -> 0.0
                        else -> result
                    }
                }
            }
        }
    }
}
