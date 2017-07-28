package core.procedures.math

import core.procedures.AFn
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils
import java.math.BigDecimal
import java.math.BigInteger

class Addition : AFn<Any?, Number?>(name = "+", isPure = true, restArgsType = Number::class.java) {

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        0    -> 0L
        1    -> args[0] as Number?
        else -> args.reduce { f, s -> Companion.add(f!! as Number, s!! as Number) } as Number
    }

    companion object {

        fun add(first: Number?, second: Number?): Number? {
            val (f, s) = Utils.upcast(first!!, second!!)
            return when {
                /* Special cases */
                Utils.isPositiveInfinity(f) && Utils.isNegativeInfinity(s) -> Double.NaN
                Utils.isPositiveInfinity(s) && Utils.isNegativeInfinity(f) -> Double.NaN
                !Utils.isFinite(f)                 -> f
                !Utils.isFinite(s)                 -> s
                Utils.isZero(f)                    -> Utils.inexactnessTaint(s, f)
                Utils.isZero(s)                    -> Utils.inexactnessTaint(f, s)
                f is BigComplex && s is BigComplex -> f.plus(s)
                f is BigRatio   && s is BigRatio   -> f.plus(s)
                f is BigDecimal && s is BigDecimal -> f.add(s)
                f is BigInteger && s is BigInteger -> f.add(s)
                f is Double     && s is Double     -> f + s
                f is Float      && s is Float      -> f + s
                else -> {
                    val fl = f.toLong()
                    val sl = s.toLong()
                    try {
                        return Math.addExact(fl, sl)
                    } catch (e: ArithmeticException) {
                        return BigInteger.valueOf(fl).add(BigInteger.valueOf(sl))
                    }
                }
            }
        }
    }
}
