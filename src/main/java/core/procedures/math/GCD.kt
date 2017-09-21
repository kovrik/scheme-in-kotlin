package core.procedures.math

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.scm.BigRatio
import core.scm.Type
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

class GCD : AFn<Any?, Number>(name = "gcd", isPure = true, restArgsType = Type.Rational::class.java) {


    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        0    -> 0L
        1    -> abs(args[0]!! as Number)
        else -> args.fold(args[0]!! as Number) { r, n -> gcd(r, n!! as Number) }
    }

    companion object {

        private const val NAME = "gcd"

        private val toInexact = ToInexact()
        private val toExact   = ToExact()
        private val lcm       = LCM()
        private val abs       = Abs()

        internal fun gcd(a: Long, b: Long): Long {
            var alocal = Math.abs(a)
            var blocal = Math.abs(b)
            while (blocal > 0) {
                val temp = blocal
                blocal = alocal % blocal
                alocal = temp
            }
            return alocal
        }

        internal fun gcd(a: Double, b: Double) = when {
            !a.isFinite() -> throw WrongTypeException(NAME, "Integer", a)
            !b.isFinite() -> throw WrongTypeException(NAME, "Integer", b)
            a.toLong().compareTo(a) != 0 || b.toLong().compareTo(b) != 0 -> toInexact(gcd(toExact(a), toExact(b)))
            else -> gcd(a.toLong(), b.toLong()).toDouble()
        }

        private fun gcd(a: BigDecimal, b: BigDecimal) = when {
            maxOf(a.scale(), b.scale()) == 0 -> BigDecimal(a.toBigInteger().gcd(b.toBigInteger()))
            else -> toInexact(gcd(toExact(a), toExact(b)))
        }

        internal fun gcd(a: BigInteger, b: BigInteger) = a.gcd(b)

        private fun gcd(first: BigRatio, second: BigRatio) = BigRatio.valueOf(first.numerator.gcd(second.numerator),
                                                                              lcm.lcm(first.denominator, second.denominator))

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
