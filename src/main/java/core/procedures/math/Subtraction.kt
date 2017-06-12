package core.procedures.math

import core.procedures.AFn
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

class Subtraction : AFn<Any?, Number?>(name = "-", isPure = true, minArgs = 1, restArgsType = Number::class.java) {

    override operator fun invoke(vararg args: Any?): Number? {
        if (args.size == 1) {
            when {
                args[0] is BigDecimal -> return  (args[0] as BigDecimal).negate()
                args[0] is BigInteger -> return  (args[0] as BigInteger).negate()
                args[0] is BigRatio   -> return -(args[0] as BigRatio)
                args[0] is Long       -> try {
                    return Math.negateExact(args[0] as Long)
                } catch (e: ArithmeticException) {
                    return BigInteger.valueOf(args[0] as Long).negate()
                }
            }
            if (args[0] is Int) {
                try {
                    return Math.negateExact(args[0] as Int)
                } catch (e: ArithmeticException) {
                    return Math.negateExact((args[0] as Int).toLong())
                }
            }
            if (Utils.isPositiveInfinity(args[0] as Number)) {
                return Double.NEGATIVE_INFINITY
            }
            if (Utils.isNegativeInfinity(args[0] as Number)) {
                return Double.POSITIVE_INFINITY
            }
            return subtract(0L, args[0] as Number)
        }
        var result = args[0]
        for (i in 1..args.size - 1) {
            result = subtract(result as Number, args[i] as Number)
        }
        return result as Number?
    }

    private fun subtract(first: Number, second: Number): Number? {
        val (f, s) = Utils.upcast(first, second)
        return when {
            /* Special cases */
            Utils.isPositiveInfinity(f) && Utils.isNegativeInfinity(s) -> Double.NaN
            Utils.isPositiveInfinity(s) && Utils.isNegativeInfinity(f) -> Double.NaN
            !Utils.isFinite(f)                 -> f
            !Utils.isFinite(s)                 -> s
            Utils.isZero(f)                    -> Utils.inexactnessTaint(f, s)
            Utils.isZero(s)                    -> Utils.inexactnessTaint(f, s)
            f is BigComplex && s is BigComplex -> f.minus(s)
            f is BigRatio   && s is BigRatio   -> f.minus(s)
            f is BigDecimal && s is BigDecimal -> f.subtract(s)
            f is BigInteger && s is BigInteger -> f.subtract(s)
            f is Double     && s is Double     -> f - s
            f is Float      && s is Float      -> f - s
            else -> {
                val fl = f.toLong()
                val sl = s.toLong()
                try {
                    return Math.subtractExact(fl, sl)
                } catch (e: ArithmeticException) {
                    return BigInteger.valueOf(fl).subtract(BigInteger.valueOf(sl))
                }
            }
        }
    }
}
