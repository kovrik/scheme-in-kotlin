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
            var first = first!!
            var second = second!!
            /* Special cases */
            if (Utils.isZero(first)) {
                return Utils.inexactnessTaint(first, second)
            }
            if (Utils.isZero(second)) {
                return Utils.inexactnessTaint(second, first)
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
                when {
                    !first.isFinite()  -> return first
                    !second.isFinite() -> return second
                }
                val result = first.toFloat() * second.toFloat()
                if (!result.isFinite()) {
                    return Utils.toBigDecimal(first).multiply(Utils.toBigDecimal(second))
                }
                return result
            }
            if (first is Double || second is Double || first is Float || second is Float) {
                when {
                    first  is Double && !first.isFinite()  -> return first
                    second is Double && !second.isFinite() -> return second
                    first  is Float  && !first.isFinite()  -> return first
                    second is Float  && !second.isFinite() -> return second
                }
                val result = first.toDouble() * second.toDouble()
                if (!result.isFinite()) {
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
