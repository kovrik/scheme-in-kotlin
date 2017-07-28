package core.procedures.math

import core.procedures.AFn
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

class Subtraction : AFn<Any?, Number?>(name = "-", isPure = true, minArgs = 1, restArgsType = Number::class.java) {

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        1 -> when {
            args[0] == null -> null
            Utils.isPositiveInfinity(args[0] as Number) -> Double.NEGATIVE_INFINITY
            Utils.isNegativeInfinity(args[0] as Number) -> Double.POSITIVE_INFINITY
            args[0] is BigDecimal -> (args[0] as BigDecimal).negate()
            args[0] is BigInteger -> (args[0] as BigInteger).negate()
            args[0] is BigRatio -> -(args[0] as BigRatio)
            args[0] is Long -> try {
                Math.negateExact(args[0] as Long) as Number
            } catch (e: ArithmeticException) {
                BigInteger.valueOf(args[0] as Long).negate()
            }
            args[0] is Int -> try {
                Math.negateExact(args[0] as Int) as Number
            } catch (e: ArithmeticException) {
                Math.negateExact((args[0] as Int).toLong())
            }
            else -> subtract(0L, args[0] as Number)
        }
        else -> args.reduce { f, s -> subtract(f!! as Number, s!! as Number) } as Number
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
