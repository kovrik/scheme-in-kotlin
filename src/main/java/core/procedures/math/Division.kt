package core.procedures.math

import core.procedures.AFn
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils
import java.math.BigDecimal

class Division : AFn<Any?, Number?>(name = "/", isPure = true, minArgs = 1, restArgsType = Number::class.java) {

    companion object {
        /* Rolls back to DEFAULT_CONTEXT if result cannot be represented with UNLIMITED precision */
        fun safeBigDecimalDivision(num: BigDecimal, den: BigDecimal): BigDecimal =
            try {
                num.divide(den, Utils.getMathContext(num, den))
            } catch (e: ArithmeticException) {
                num.divide(den, Utils.DEFAULT_CONTEXT)
            }
    }

    override operator fun invoke(vararg args: Any?): Number? {
        if (args.size == 1) {
            return invoke(1L, args[0] as Number)
        }
        var result = args[0] as Number
        for (d in 1..args.size - 1) {
            result = invoke(result, args[d]!! as Number)
        }
        return result
    }

    private operator fun invoke(arg1: Number?, arg2: Number?): Number {
        val (n, d) = Utils.upcast(arg1, arg2)
        return when {
            Utils.isZero(d)  && Utils.isExact(d) -> throw ArithmeticException("Division by zero")
            Utils.isPositiveInfinity(d)          ->  0.0
            Utils.isNegativeInfinity(d)          -> -0.0
            Utils.isZero(n)  && Utils.isExact(n) -> Utils.inexactnessTaint(n, d)
            n is BigComplex  && d is BigComplex  -> n / d
            n is BigRatio    && d is BigRatio    -> n / d
            n is BigDecimal  && d is BigDecimal  -> n.divide(d)
            n is Double      && d is Double      -> n / d
            n is Float       && d is Float       -> n / d
            Utils.isExact(n) && Utils.isExact(d) -> BigRatio.valueOf(Utils.toBigInteger(n), Utils.toBigInteger(d))
            else                                 -> n.toDouble() / d.toDouble()
        }
    }
}
