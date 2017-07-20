package core.procedures.math

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.scm.BigRatio
import core.scm.Type
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

class GCD : AFn<Any?, Number>(name = "gcd", isPure = true, restArgsType = Type.Rational::class.java) {

    override operator fun invoke(args: Array<out Any?>): Number {
        if (args.isEmpty()) {
            return 0L
        }
        if (args.size == 1) {
            return Abs.abs(args[0]!! as Number)
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
                val aex = ToExact.toExact(a)
                val bex = ToExact.toExact(b)
                return ToInexact.toInexact(gcd(aex, bex))
            }
            return gcd(a.toLong(), b.toLong()).toDouble()
        }

        internal fun gcd(a: BigDecimal, b: BigDecimal): Number {
            val scale = maxOf(a.scale(), b.scale())
            if (scale == 0) {
                return BigDecimal(a.toBigInteger().gcd(b.toBigInteger()))
            } else {
                return ToInexact.toInexact(gcd(ToExact.toExact(a), ToExact.toExact(b)))
            }
        }

        internal fun gcd(a: BigInteger, b: BigInteger) = a.gcd(b)

        internal fun gcd(first: BigRatio, second: BigRatio) = BigRatio.valueOf(first.numerator.gcd(second.numerator),
                                                                               LCM.lcm(first.denominator, second.denominator))

        fun gcd(first: Number, second: Number): Number {
            val (f, s) = Utils.upcast(first, second)
            return when {
                f is Double     && s is Double     -> gcd(f, s)
                f is Float      && s is Float      -> gcd(f.toDouble(), s.toDouble())
                f is BigRatio   && s is BigRatio   -> gcd(f, s)
                f is BigDecimal && s is BigDecimal -> gcd(f, s)
                f is BigInteger && s is BigInteger -> gcd(f, s)
                else                               -> gcd(f.toLong(), s.toLong())
            }
        }
    }
}
