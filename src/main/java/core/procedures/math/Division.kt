package core.procedures.math

import core.procedures.AFn
import core.procedures.Arity.AtLeast
import core.scm.Complex
import core.scm.Ratio
import core.utils.Utils
import core.utils.Utils.taint
import java.math.BigDecimal

class Division : AFn<Any?, Number?>(name = "/", isPure = true, arity = AtLeast(1), restArgsType = Number::class.java) {

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        1    -> invoke(1L, args[0]!! as Number)
        else -> args.reduce { f, s -> invoke(f!! as Number, s!! as Number) } as Number
    }

    private operator fun invoke(arg1: Number, arg2: Number): Number {
        val (n, d) = Utils.upcast(arg1, arg2)
        return when {
            Utils.isZero(d)  && Utils.isExact(d) -> throw ArithmeticException("Division by zero")
            Utils.isPositiveInfinity(d)          ->  0.0
            Utils.isNegativeInfinity(d)          -> -0.0
            Utils.isZero(n)  && Utils.isExact(n) -> d taint n
            n is Complex  && d is Complex  -> n / d
            n is Ratio    && d is Ratio    -> n / d
            n is BigDecimal  && d is BigDecimal  -> n.divide(d)
            n is Double      && d is Double      -> n / d
            n is Float       && d is Float       -> n / d
            Utils.isExact(n) && Utils.isExact(d) -> Ratio.valueOf(Utils.toBigInteger(n), Utils.toBigInteger(d))
            else                                 -> n.toDouble() / d.toDouble()
        }
    }
}
