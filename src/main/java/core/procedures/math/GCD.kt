package core.procedures.math

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.BigRatio
import core.utils.Utils

import java.lang.NullPointerException
import java.math.BigDecimal
import java.math.BigInteger

class GCD : AFn(FnArgsBuilder().rest(BigRatio::class.java).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = NAME

    override fun apply(args: Array<Any?>): Number? {
        if (args.isEmpty()) {
            return 0L
        }
        if (args.size == 1) {
            return ABS.apply1(args[0])
        }
        if (args[0] == null) throw NullPointerException()
        var result = args[0] as Number
        for (i in 1..args.size - 1) {
            if (args[i] == null) throw NullPointerException()
            result = gcd(result, args[i] as Number)
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
            if (a.isInfinite() || a.isNaN()) {
                throw WrongTypeException(NAME, "Integer", a)
            }
            if (b.isInfinite() || b.isNaN()) {
                throw WrongTypeException(NAME, "Integer", b)
            }
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
            val f = Utils.upcast(first)
            val s = Utils.upcast(second)
            if (f is Long && s is Long) {
                return gcd(f, s)
            }
            if (first is BigRatio && second is BigRatio) {
                return gcd(first, second)
            }
            if (first is BigRatio) {
                return gcd(first.toBigDecimal(), Utils.toBigDecimal(second))
            }
            if (second is BigRatio) {
                return gcd(Utils.toBigDecimal(first), second.toBigDecimal())
            }
            if (first is BigDecimal || second is BigDecimal) {
                return gcd(Utils.toBigDecimal(first), Utils.toBigDecimal(second))
            }
            if (first is BigInteger || second is BigInteger) {
                return gcd(Utils.toBigInteger(first), Utils.toBigInteger(second))
            }
            return gcd(first.toDouble(), second.toDouble())
        }
    }
}
