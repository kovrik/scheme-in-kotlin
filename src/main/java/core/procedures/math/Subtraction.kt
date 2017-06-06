package core.procedures.math

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

class Subtraction : AFn(FnArgs(min = 1, rest = Number::class.java)) {

    override val isPure = true
    override val name = "-"

    override operator fun invoke(vararg args: Any?): Any? {
        if (args.size == 1) {
            when {
                args[0] is BigDecimal -> return (args[0] as BigDecimal).negate()
                args[0] is BigInteger -> return (args[0] as BigInteger).negate()
                args[0] is BigRatio   -> return (args[0] as BigRatio).negate()
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
        return result
    }

    private fun subtract(first: Number, second: Number): Number? {
        val (f, s) = Utils.upcast(first, second)
        when {
            /* Special cases */
            Utils.isPositiveInfinity(f) && Utils.isNegativeInfinity(s) -> return Double.NaN
            Utils.isPositiveInfinity(s) && Utils.isNegativeInfinity(f) -> return Double.NaN
            !Utils.isFinite(f)                                         -> return f
            !Utils.isFinite(s)                 -> return s
            Utils.isZero(f)                    -> return Utils.inexactnessTaint(f, s)
            Utils.isZero(s)                    -> return Utils.inexactnessTaint(f, s)
            f is BigComplex && s is BigComplex -> return f.minus(s)
            f is BigRatio   && s is BigRatio   -> return f.minus(s)
            f is BigDecimal && s is BigDecimal -> return f.subtract(s)
            f is BigInteger && s is BigInteger -> return f.subtract(s)
            f is Double     && s is Double     -> return f - s
            f is Float      && s is Float      -> return f - s
            else -> {
                val f = f.toLong()
                val s = s.toLong()
                try {
                    return Math.subtractExact(f, s)
                } catch (e: ArithmeticException) {
                    return BigDecimal.valueOf(f).subtract(BigDecimal.valueOf(s))
                }
            }
        }
    }
}
