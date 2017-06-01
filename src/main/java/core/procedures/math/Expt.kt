package core.procedures.math

import core.procedures.AFn
import core.procedures.FnArgs
import core.procedures.predicates.Predicate
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils
import core.writer.Writer
import java.math.BigDecimal
import java.math.BigInteger

class Expt : AFn(FnArgs(min = 2, max = 2, mandatory = arrayOf<Class<*>>(Number::class.java, Number::class.java))) {

    override val isPure = true
    override val name = "expt"

    override operator fun invoke(arg1: Any?, arg2: Any?) = expt(arg1 as Number?, arg2 as Number?)

    companion object {
        /* TODO: Optimize Special Cases! */
        fun expt(base: Number?, exponent: Number?): Number? {
            base!!
            exponent!!
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
            if ((base is Double && base.isNaN()) || (exponent is Double && exponent.isNaN())) {
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
                        throw ArithmeticException("expt: undefined for $base and ${Writer.write(exponent)}")
                    } else {
                        return 0L
                    }
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

            /* Complex numbers */
            if (base is BigComplex || exponent is BigComplex) {
                return BigComplex.of(base).expt(BigComplex.of(exponent))
            }
            /* Long, Integer, Short, Byte */
            val b = Utils.upcast(base)
            val ex = Utils.upcast(exponent)
            if (b is Long && ex is Long) {
                var isNegative = false
                if (exponent.toLong() < Int.MAX_VALUE) {
                    var e = exponent.toInt()
                    if (e < 0) {
                        isNegative = true
                        e = Math.abs(e)
                    }
                    val result = BigInteger.valueOf(base.toLong()).pow(e)
                    if (isNegative) {
                        return BigRatio.valueOf(BigInteger.ONE, result)
                    }
                    return Utils.downcastNumber(result)
                } else {
                    /* If we came here, then exponent is greater than Int.MAX_VALUE */
                    if (Math.abs(base.toLong()) < 1) {
                        return 0L
                    }
                    if (base.toLong() > 0) {
                        return Double.POSITIVE_INFINITY
                    } else {
                        return if (Predicate.IS_ODD(exponent)) Double.NEGATIVE_INFINITY else Double.POSITIVE_INFINITY
                    }
                }
            }
            /* BigIntegers */
            if (base is BigInteger && Utils.isInteger(exponent)) {
                if (Utils.isInteger(base)) {
                    if (exponent is BigInteger) {
                        try {
                            return base.pow(exponent.intValueExact())
                        } catch (e: ArithmeticException) {
                            // ignore
                        }
                    }
                }
                return exptBigInt(Utils.toBigInteger(base), Utils.toBigInteger(exponent))
            }
            /* BigDecimals */
            if (base is BigDecimal && Utils.isInteger(exponent)) {
                if (Utils.isInteger(base)) {
                    if (exponent is BigDecimal) {
                        try {
                            return base.pow(exponent.intValueExact())
                        } catch (e: ArithmeticException) {
                            return exptBigDec(base, exponent)
                        }
                    }
                }
                return exptBigDec(base, Utils.toBigDecimal(exponent))
            }
            /* Double */
            val result = Math.pow(base.toDouble(), exponent.toDouble())
            if (result == Double.POSITIVE_INFINITY || result == Double.NEGATIVE_INFINITY) {
                return Utils.toBigDecimal(base).pow(exponent.toInt())
            }
            if (result is Double && result.isNaN()) {
                return BigComplex.of(base).expt(BigComplex.of(exponent))
            }
            return result
        }

        private fun exptBigInt(n: BigInteger, e: BigInteger): Number {
            try {
                val i = e.intValueExact()
                return n.pow(i)
            } catch (ex: ArithmeticException) {
                // FIXME NEGATIVE_INFINITY and zero in some cases?
                return Double.POSITIVE_INFINITY
            }
        }

        private fun exptBigDec(n: BigDecimal, e: BigDecimal): Number {
            try {
                val scale = Math.max(n.scale(), n.stripTrailingZeros().scale())
                val i = e.intValueExact()
                return n.pow(i).setScale(scale, Utils.ROUNDING_MODE)
            } catch (ex: ArithmeticException) {
                // FIXME NEGATIVE_INFINITY and zero in some cases?
                return Double.POSITIVE_INFINITY
            }
        }
    }
}
