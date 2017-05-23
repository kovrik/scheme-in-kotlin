package core.procedures.math

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.BigRatio
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

class LCM : AFn(FnArgsBuilder().rest(BigRatio::class.java).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "lcm"

    override fun apply(args: Array<Any?>): Number? {
        if (args.isEmpty()) {
            return 1L
        }
        if (args.size == 1) {
            return ABS.apply1(args[0])
        }
        var result = args[0] as Number
        for (i in 1..args.size - 1) {
            result = lcm(result, args[i] as Number)
        }
        return result
    }

    private fun lcm(first: BigRatio, second: BigRatio): BigRatio {
        return BigRatio.valueOf(Companion.lcm(first.numerator, second.numerator),
                                      GCD.gcd(first.denominator, second.denominator))
    }

    private fun lcm(a: BigDecimal, b: BigDecimal): Number {
        if (BigDecimal.ZERO.compareTo(a) == 0 && BigDecimal.ZERO.compareTo(b) == 0) {
            return BigDecimal.ZERO
        }
        val scale = Math.max(a.scale(), b.scale())
        if (scale == 0) {
            return BigDecimal(Companion.lcm(a.toBigInteger(), b.toBigInteger()))
        } else {
            return ToInexact.toInexact(lcm(ToExact.toExact(a) as BigDecimal, ToExact.toExact(b) as BigDecimal))
        }
    }

    private fun lcm(first: Number, second: Number): Number {
        val f = Utils.upcast(first)
        val s = Utils.upcast(second)
        if (f is Long && s is Long) {
            return Companion.lcm(first as Long, second as Long)
        }
        if (first is BigRatio && second is BigRatio) {
            return lcm(first, second)
        }
        if (first is BigRatio) {
            return lcm(first.toBigDecimal(), Utils.toBigDecimal(second))
        }
        if (second is BigRatio) {
            return lcm(Utils.toBigDecimal(first), second.toBigDecimal())
        }
        if (first is BigDecimal || second is BigDecimal) {
            return lcm(Utils.toBigDecimal(first), Utils.toBigDecimal(second))
        }
        if (first is BigInteger || second is BigInteger) {
            return Companion.lcm(Utils.toBigInteger(first), Utils.toBigInteger(second))
        }
        return Companion.lcm(first.toDouble(), second.toDouble())
    }

    companion object {

        private val ABS = Abs()

        private fun lcm(a: Long, b: Long): Long {
            if (a.toInt() == 0 && b.toInt() == 0) {
                return 0L
            }
            return a / GCD.gcd(a, b) * b
        }

        private fun lcm(a: Double, b: Double): Double {
            if (a.toInt() == 0 && b.toInt() == 0) {
                return 0.0
            }
            return a / GCD.gcd(a, b).toDouble() * b
        }

        internal fun lcm(first: BigInteger, second: BigInteger): BigInteger {
            return first.multiply(second.divide(GCD.gcd(first, second)))
        }
    }
}
