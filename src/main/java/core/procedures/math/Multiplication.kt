package core.procedures.math

import core.procedures.AFn
import core.scm.Complex
import core.scm.Ratio
import core.utils.Utils
import core.utils.Utils.taint

import java.math.BigDecimal
import java.math.BigInteger

class Multiplication : AFn<Any?, Number?>(name = "*", isPure = true, restArgsType = Number::class.java) {

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        0    -> 1L
        1    -> args[0] as Number?
        else -> args.reduce { f, s -> invoke(f!! as Number, s!! as Number) } as Number
    }

    operator fun invoke(first: Number?, second: Number?): Number {
        val (f, s) = Utils.upcast(first!!, second!!)
        return when {
            Utils.isZero(f) -> when {
                !Utils.isFinite(s) && Utils.isInexact(f) -> Double.NaN
                else -> s taint f
            }
            Utils.isZero(s) -> when {
                !Utils.isFinite(f) && Utils.isInexact(s) -> Double.NaN
                else -> f taint s
            }
            Utils.isOne(f) -> f taint s
            Utils.isOne(s) -> s taint f
            f is Complex    && s is Complex    -> f * s
            f is Ratio      && s is Ratio      -> f * s
            f is BigDecimal && s is BigDecimal -> f * s
            f is BigInteger && s is BigInteger -> f * s
            f is Double     && s is Double     -> f * s
            f is Float      && s is Float      -> f * s
            else -> {
                val fl = f.toLong()
                val sl = s.toLong()
                return try {
                    Math.multiplyExact(fl, sl)
                } catch (e: ArithmeticException) {
                    fl.toBigInteger() * sl.toBigInteger()
                }
            }
        }
    }
}
