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
        return if (Math.max(a.scale(), b.scale()) == 0) {
            BigDecimal(Companion.lcm(a.toBigInteger(), b.toBigInteger()))
        } else {
            ToInexact.toInexact(lcm(ToExact.toExact(a) as BigDecimal, ToExact.toExact(b) as BigDecimal))
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
        val (f, s) = Utils.upcast(first, second)
        return when {
            f is Double     && s is Double     -> lcm(f, s)
            f is Float      && s is Float      -> lcm(f.toDouble(), s.toDouble())
            f is BigRatio   && s is BigRatio   -> lcm(f, s)
            f is BigDecimal && s is BigDecimal -> lcm(f, s)
            f is BigInteger && s is BigInteger -> Companion.lcm(f, s)
            else                               -> lcm(f.toLong(), s.toLong())
        }
    }

    companion object {
        internal fun lcm(first: BigInteger, second: BigInteger) = first.multiply(second.divide(GCD.gcd(first, second)))
    }
}
