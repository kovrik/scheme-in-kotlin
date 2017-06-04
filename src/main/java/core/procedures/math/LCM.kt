package core.procedures.math

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.BigRatio
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

class LCM : AFn(FnArgs(rest = BigRatio::class.java)) {

    private val ABS = Abs()

    override val isPure = true
    override val name = "lcm"

    override operator fun invoke(vararg args: Any?): Number? {
        if (args.isEmpty()) {
            return 1L
        }
        if (args.size == 1) {
            return ABS(args[0])
        }
        args[0]!!
        var result = args[0] as Number
        for (i in 1..args.size - 1) {
            result = lcm(result, args[i]!! as Number)
        }
        return result
    }

    private fun lcm(first: BigRatio, second: BigRatio) = BigRatio.valueOf(Companion.lcm(first.numerator, second.numerator),
                                                                          GCD.gcd(first.denominator, second.denominator))

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

    private fun lcm(first: Number, second: Number): Number {
        val f = Utils.upcast(first)!!
        val s = Utils.upcast(second)!!
        return when {
            f is Long && s is Long -> lcm(f, s)
            f is BigRatio && s is BigRatio -> lcm(f, s)
            f is BigRatio -> lcm(f.toBigDecimal(), Utils.toBigDecimal(s))
            s is BigRatio -> lcm(Utils.toBigDecimal(f), s.toBigDecimal())
            f is BigDecimal || s is BigDecimal -> lcm(Utils.toBigDecimal(f), Utils.toBigDecimal(s))
            f is BigInteger || s is BigInteger -> Companion.lcm(Utils.toBigInteger(f), Utils.toBigInteger(s))
            else -> lcm(f.toDouble(), s.toDouble())
        }
    }

    companion object {
        internal fun lcm(first: BigInteger, second: BigInteger) = first.multiply(second.divide(GCD.gcd(first, second)))
    }
}
