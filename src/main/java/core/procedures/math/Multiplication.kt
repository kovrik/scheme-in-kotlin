package core.procedures.math

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

class Multiplication : AFn(FnArgsBuilder().rest(Number::class.java).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "*"

    override operator fun invoke(vararg args: Any?): Number? {
        when (args.size) {
            0 -> return 1L
            1 -> return args[0] as Number
            else -> {
                var result: Any? = 1L
                for (arg in args) {
                    if (arg == null) throw NullPointerException()
                    result = Companion(result as Number?, arg as Number?)
                }
                return result as Number
            }
        }
    }

    companion object {

        operator fun invoke(first: Number?, second: Number?): Number {
            if (first  == null) throw NullPointerException()
            if (second == null) throw NullPointerException()
            var first = first
            var second = second
            /* Special cases */
            if (Utils.isZero(first)) {
                return Utils.inexactnessTaint(first, second)
            }
            if (Utils.isOne(first)) {
                return Utils.inexactnessTaint(second, first)
            }
            if (Utils.isOne(second)) {
                return Utils.inexactnessTaint(first, second)
            }
            /* Complex numbers*/
            if (first is BigComplex) {
                return first.multiply(second)
            }
            if (second is BigComplex) {
                return second.multiply(first)
            }
            /* Big Ratio numbers */
            if (first is BigRatio && second is BigRatio) {
                return first.multiply(second)
            }
            if (first is BigRatio) {
                if (Utils.isExact(second)) {
                    return first.multiply(Utils.toBigInteger(second))
                } else {
                    first = first.toDouble()
                }
            }
            if (second is BigRatio) {
                if (Utils.isExact(first)) {
                    return second.multiply(Utils.toBigInteger(first))
                } else {
                    second = second.toDouble()
                }
            }
            if (first is Float && second is Float) {
                val result = first.toFloat() * second.toFloat()
                if (java.lang.Float.isNaN(result) || java.lang.Float.isInfinite(result)) {
                    return Utils.toBigDecimal(first).multiply(Utils.toBigDecimal(second))
                }
                return result
            }
            if (first is Double || second is Double || first is Float || second is Float) {
                val result = first.toDouble() * second.toDouble()
                if (java.lang.Double.isNaN(result) || java.lang.Double.isInfinite(result)) {
                    return Utils.toBigDecimal(first).multiply(Utils.toBigDecimal(second))
                }
                return result
            }
            if (first is BigDecimal || second is BigDecimal) {
                return Utils.toBigDecimal(first).multiply(Utils.toBigDecimal(second))
            }
            if (first is BigInteger || second is BigInteger) {
                return Utils.toBigInteger(first).multiply(Utils.toBigInteger(second))
            }
            val f = first.toLong()
            val s = second.toLong()
            try {
                return Math.multiplyExact(f, s)
            } catch (e: ArithmeticException) {
                return BigDecimal.valueOf(f).multiply(BigDecimal.valueOf(s))
            }
        }
    }
}
