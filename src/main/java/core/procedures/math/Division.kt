package core.procedures.math

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils
import java.math.BigDecimal

class Division : AFn(FnArgs(min = 1, rest = Number::class.java)) {

    companion object {
        /* Rolls back to DEFAULT_CONTEXT if result cannot be represented with UNLIMITED precision */
        fun safeBigDecimalDivision(num: BigDecimal, den: BigDecimal): BigDecimal {
            try {
                return num.divide(den, Utils.getMathContext(num, den))
            } catch (e: ArithmeticException) {
                return num.divide(den, Utils.DEFAULT_CONTEXT)
            }
        }
    }

    override val isPure = true
    override val name = "/"

    override operator fun invoke(vararg args: Any?): Number? {
        if (args.size == 1) {
            return invoke(1L, args[0] as Number)
        }
        var result = args[0] as Number?
        for (d in 1..args.size - 1) {
            result = invoke(result, args[d] as Number)
        }
        return result
    }

    private operator fun invoke(numerator: Number?, denominator: Number?): Number? {
        var (n, d) = Utils.upcast(numerator, denominator)
        if (Utils.isZero(d) && Utils.isExact(d)) throw ArithmeticException("Division by zero")
        if (Utils.isPositiveInfinity(d)) return  0.0
        if (Utils.isNegativeInfinity(d)) return -0.0
        if (Utils.isZero(n)) {
            if (Utils.isZero(d) && Utils.isInexact(n) && Utils.isInexact(d)) {
                return Double.NaN
            }
            return Utils.inexactnessTaint(n, d)
        }
        when {
            /* Special cases */
            n is BigComplex  && d is BigComplex  -> return n.divide(d)
            n is BigRatio    && d is BigRatio    -> return n.divide(d)
            n is BigDecimal  && d is BigDecimal  -> return n.divide(d)
            Utils.isExact(n) && Utils.isExact(d) -> return BigRatio.valueOf(Utils.toBigInteger(n), Utils.toBigInteger(d))
            n is Double      && d is Double      -> return n / d
            n is Float       && d is Float       -> return n / d
            else                                 -> return n.toDouble() / d.toDouble()
        }
    }
}
