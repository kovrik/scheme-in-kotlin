package core.procedures.math

import core.procedures.AFn
import core.procedures.predicates.Predicate
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils
import core.writer.Writer
import java.math.BigDecimal
import java.math.BigInteger
import kotlin.math.absoluteValue
import kotlin.math.pow

class Expt : AFn<Number?, Number>(name = "expt", isPure = true, minArgs = 2, maxArgs = 2,
                                  mandatoryArgsTypes = arrayOf(Number::class.java, Number::class.java)) {

    override operator fun invoke(arg1: Number?, arg2: Number?): Number {
        val base = arg1!!
        val exponent = arg2!!
        /* Special cases
         *
         * Special cases when w is a real number:
         * These special cases correspond to pow in C99 [C99], except when z is negative and w is a not an integer.
         * See: https://docs.racket-lang.org/reference/generic-numbers.html#(def._((quote._~23~25kernel)._expt))
         */
        /*
         * (expt 0.0 w):
         *
         *  w is negative — +inf.0
         *  w is positive — 0.0
         *
         * (expt -0.0 w):
         *
         *  w is negative:
         *  w is an odd integer — -inf.0
         *
         *  w otherwise rational — +inf.0
         *
         *  w is positive:
         *  w is an odd integer — -0.0
         *
         *  w otherwise rational — 0.0
         */
        if (Utils.isNaN(base) || Utils.isNaN(exponent)) {
            return Double.NaN
        }
        if (Utils.isZero(base) && Utils.isZero(exponent)) {
            return Utils.inexactnessTaint(1L, exponent)
        }
        if (Utils.isZero(base) && Utils.isFinite(exponent)) {
            if (base == -0.0) {
                if (Utils.isNegative(exponent)) {
                    return if (Utils.isInteger(exponent) && Predicate.IS_ODD(exponent))
                        Double.NEGATIVE_INFINITY
                    else
                        Double.POSITIVE_INFINITY
                } else {
                    return if (Utils.isInteger(exponent) && Predicate.IS_ODD(exponent)) -0.0 else 0.0
                }
            }
            return if (Utils.isNegative(exponent))
                Double.POSITIVE_INFINITY
            else
                Utils.inexactnessTaint(0L, base)
        }
        if (Utils.isOne(base)) {
            return Utils.inexactnessTaint(base, exponent)
        }
        if (Utils.isZero(exponent)) {
            return 1L
        }
        if (Utils.isOne(exponent)) {
            return base
        }
        /* Special cases for Real numbers */
        /*
         * (expt -inf.0 w) for integer w:
         *  w is negative:
         *   w is odd  — -0.0
         *   w is even —  0.0
         *
         *  w is positive:
         *   w is odd  — -inf.0
         *   w is even — +inf.0
         */
        if (Double.NEGATIVE_INFINITY == base) {
            if (Utils.isInteger(exponent)) {
                if (Utils.isNegative(exponent)) {
                    return if (Predicate.IS_ODD(exponent)) -0.0 else 0.0
                } else {
                    return if (Predicate.IS_ODD(exponent)) Double.NEGATIVE_INFINITY else Double.POSITIVE_INFINITY
                }
            }
        }
        /* (expt +inf.0 w):
         *  w is negative — 0.0
         *  w is positive — +inf.0
         */
        if (Double.POSITIVE_INFINITY == base) {
            return if (Utils.isPositive(exponent)) Double.POSITIVE_INFINITY else 0.0
        }
        /* (expt z -inf.0) for positive z:
         *
         *   z is less than 1.0 — +inf.0
         *   z is greater than 1.0 — 0.0
         *
         * (expt z +inf.0) for positive z:
         *
         *  z is less than 1.0 — 0.0
         *  z is greater than 1.0 — +inf.0
         */
        if (exponent == Double.POSITIVE_INFINITY || exponent == Double.NEGATIVE_INFINITY) {
            if (base is BigComplex) {
                return Double.NaN
            }
            if (Utils.isZero(base)) {
                if (exponent == Double.NEGATIVE_INFINITY) {
                    if (Utils.isInexact(base)) {
                        return Double.POSITIVE_INFINITY
                    }
                    throw ArithmeticException("expt: undefined for $base and ${Writer.write(exponent)}")
                }
                return 0L
            }
            if (exponent == Double.NEGATIVE_INFINITY) {
                if (NumericalComparison.LESS(base, 1L)) {
                    return Double.POSITIVE_INFINITY
                } else if (NumericalComparison.GREATER(base, 1L)) {
                    return 0.0
                }
            } else if (exponent == Double.POSITIVE_INFINITY) {
                if (NumericalComparison.LESS(base, 1L)) {
                    return 0.0
                } else if (NumericalComparison.GREATER(base, 1L)) {
                    return Double.POSITIVE_INFINITY
                }
            }
        }
        val (b, ex) = Utils.upcast(base, exponent)
        /* Complex numbers */
        if (b is BigComplex && ex is BigComplex) {
            return b.expt(ex)
        }
        /* BigIntegers */
        if (b is BigInteger && ex is BigInteger) {
            return exptBigInt(b, ex)
        }
        /* BigDecimals (only if they are integral) */
        if (b is BigDecimal && ex is BigDecimal && Utils.isInteger(b) && Utils.isInteger(ex)) {
            return exptBigDec(b, ex)
        }
        /* Long, Integer, Short, Byte */
        if (b is Long && ex is Long) {
            var isNegative = false
            if (ex < Int.MAX_VALUE) {
                var e = ex.toInt()
                if (e < 0) {
                    isNegative = true
                    e = e.absoluteValue
                }
                val result = BigInteger.valueOf(b).pow(e)
                if (isNegative) {
                    return BigRatio.valueOf(BigInteger.ONE, result)
                }
                return Utils.downcastNumber(result)
            } else {
                /* If we came here, then ex is greater than Int.MAX_VALUE */
                return when {
                    b.absoluteValue < 1 -> 0L
                    b > 0 -> Double.POSITIVE_INFINITY
                    else -> if (Predicate.IS_ODD(ex)) Double.NEGATIVE_INFINITY else Double.POSITIVE_INFINITY
                }
            }
        }
        /* Double */
        val result = b.toDouble().pow(ex.toDouble())
        return when {
            result.isNaN() -> BigComplex.of(b).expt(BigComplex.of(ex))
            !result.isFinite() -> Utils.toBigDecimal(b).pow(ex.toInt())
            else -> result
        }
    }

    private fun exptBigInt(n: BigInteger, e: BigInteger): Number = try {
        n.pow(e.intValueExact())
    } catch (ex: ArithmeticException) {
        Double.POSITIVE_INFINITY
    }

    private fun exptBigDec(n: BigDecimal, e: BigDecimal): Number = try {
        n.pow(e.intValueExact()).setScale(maxOf(n.scale(), n.stripTrailingZeros().scale()), Utils.ROUNDING_MODE)
    } catch (ex: ArithmeticException) {
        Double.POSITIVE_INFINITY
    }
}
