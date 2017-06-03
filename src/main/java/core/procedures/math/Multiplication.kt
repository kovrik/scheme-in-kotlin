package core.procedures.math

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

class Multiplication : AFn(FnArgs(rest = Number::class.java)) {

    override val isPure = true
    override val name = "*"

    override operator fun invoke(vararg args: Any?): Number? {
        return when (args.size) {
            0 -> 1L
            1 -> args[0] as Number
            else -> {
                var result: Any? = 1L
                for (arg in args) {
                    result = Companion(result as Number?, arg!! as Number?)
                }
                result as Number
            }
        }
    }

    companion object {

        operator fun invoke(first: Number?, second: Number?): Number {
            var (f, s) = Utils.upcast(first!!, second!!)
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
            when {
                /* Special cases */
                Utils.isOne(f)                     -> return Utils.inexactnessTaint(s, f)
                Utils.isOne(s)                     -> return Utils.inexactnessTaint(f, s)
                f is BigComplex && s is BigComplex -> return f.multiply(s)
                f is BigRatio   && s is BigRatio   -> return f.multiply(s)
                f is BigDecimal && s is BigDecimal -> return f.multiply(s)
                f is BigInteger && s is BigInteger -> return f.multiply(s)
                f is Double     && s is Double     -> return f * s
                f is Float      && s is Float      -> return f * s
                else -> {
                    val f = f.toLong()
                    val s = s.toLong()
                    try {
                        return Math.multiplyExact(f, s)
                    } catch (e: ArithmeticException) {
                        return BigDecimal.valueOf(f).multiply(BigDecimal.valueOf(s))
                    }
                }
            }
        }
    }
}
