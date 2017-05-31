package core.procedures.math

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.BigRatio
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

class GCD : AFn(FnArgs(rest = BigRatio::class.java)) {

    override val isPure = true
    override val name = NAME

    override operator fun invoke(vararg args: Any?): Number? {
        if (args.isEmpty()) {
            return 0L
        }
        if (args.size == 1) {
            return ABS(args[0])
        }
        args[0]!!
        var result = args[0] as Number
        for (i in 1..args.size - 1) {
            result = gcd(result, args[i]!! as Number)
        }
        return result
    }

    companion object {

        private val NAME = "gcd"
        private val ABS = Abs()

        internal fun gcd(a: Long, b: Long): Long {
            var a = a
            var b = b
            while (b > 0) {
                val temp = b
                b = a % b
                a = temp
            }
            return a
        }

        internal fun gcd(a: Double, b: Double): Number {
            if (!a.isFinite()) throw WrongTypeException(NAME, "Integer", a)
            if (!b.isFinite()) throw WrongTypeException(NAME, "Integer", b)
            if (a.toLong().compareTo(a) != 0 || b.toLong().compareTo(b) != 0) {
                return ToInexact.toInexact(gcd(ToExact.toExact(a), ToExact.toExact(b)))
            }
            return gcd(a.toLong(), b.toLong()).toDouble()
        }

        internal fun gcd(a: BigDecimal, b: BigDecimal): Number {
            val scale = Math.max(a.scale(), b.scale())
            if (scale == 0) {
                return BigDecimal(a.toBigInteger().gcd(b.toBigInteger()))
            } else {
                return ToInexact.toInexact(gcd(ToExact.toExact(a), ToExact.toExact(b)))
            }
        }

        internal fun gcd(a: BigInteger, b: BigInteger): BigInteger {
            return a.gcd(b)
        }

        internal fun gcd(first: BigRatio, second: BigRatio): BigRatio {
            return BigRatio.valueOf(first.numerator.gcd(second.numerator),
                            LCM.lcm(first.denominator, second.denominator))
        }

        fun gcd(first: Number, second: Number): Number {
            val f = Utils.upcast(first)!!
            val s = Utils.upcast(second)!!
            return when {
                f is Long && s is Long -> gcd(f, s)
                f is BigRatio && s is BigRatio -> gcd(f, s)
                f is BigRatio -> gcd(f.toBigDecimal(), Utils.toBigDecimal(s))
                s is BigRatio -> gcd(Utils.toBigDecimal(f), s.toBigDecimal())
                f is BigDecimal || s is BigDecimal -> gcd(Utils.toBigDecimal(f), Utils.toBigDecimal(s))
                f is BigInteger || s is BigInteger -> gcd(Utils.toBigInteger(f), Utils.toBigInteger(s))
                else -> gcd(f.toDouble(), s.toDouble())
            }
        }
    }
}
