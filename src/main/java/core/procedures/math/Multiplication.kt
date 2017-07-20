package core.procedures.math

import core.procedures.AFn
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

class Multiplication : AFn<Any?, Number?>(name = "*", isPure = true, restArgsType = Number::class.java) {

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        0    -> 1L
        1    -> args[0] as Number?
        else -> args.fold(1L as Number?) { r, n -> Companion(r!!, n!! as Number) } as Number
    }

    companion object {

        operator fun invoke(first: Number?, second: Number?): Number {
            val (f, s) = Utils.upcast(first!!, second!!)
            /* Special cases */
            if (Utils.isZero(f)) {
                if (!Utils.isFinite(s) && Utils.isInexact(f)) {
                    return Double.NaN
                }
                return Utils.inexactnessTaint(f, s)
            }
            if (Utils.isZero(s)) {
                if (!Utils.isFinite(f) && Utils.isInexact(s)) {
                    return Double.NaN
                }
                return Utils.inexactnessTaint(s, f)
            }
            return when {
                /* Special cases */
                Utils.isOne(f)                     -> Utils.inexactnessTaint(s, f)
                Utils.isOne(s)                     -> Utils.inexactnessTaint(f, s)
                f is BigComplex && s is BigComplex -> f * s
                f is BigRatio   && s is BigRatio   -> f * s
                f is BigDecimal && s is BigDecimal -> f.multiply(s)
                f is BigInteger && s is BigInteger -> f.multiply(s)
                f is Double     && s is Double     -> f * s
                f is Float      && s is Float      -> f * s
                else -> {
                    val fl = f.toLong()
                    val sl = s.toLong()
                    try {
                        return Math.multiplyExact(fl, sl)
                    } catch (e: ArithmeticException) {
                        return BigInteger.valueOf(fl).multiply(BigInteger.valueOf(sl))
                    }
                }
            }
        }
    }
}
