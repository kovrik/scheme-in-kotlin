package core.procedures.math;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.procedures.predicates.SCMPredicate;
import core.scm.SCMBigComplex;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;
import core.writer.Writer;

import java.math.BigDecimal;
import java.math.BigInteger;

public final class Expt extends AFn {

  public Expt() {
    super(new FnArgsBuilder().minArgs(2).maxArgs(2).mandatoryArgsTypes(new Class[]{Number.class, Number.class}));
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "expt";
  }

  @Override
  public Number apply2(Object arg1, Object arg2) {
    return expt((Number)arg1, (Number)arg2);
  }

  /**
   * TODO: Optimize Special Cases!
   */
  public static Number expt(Number base, Number exponent) {
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
    if (NumberUtils.isNaN(base) || NumberUtils.isNaN(exponent)) {
      return Double.NaN;
    }
    if (NumberUtils.isZero(base) && NumberUtils.isZero(exponent)) {
      return 1L;
    }
    if (NumberUtils.isZero(base) && (NumberUtils.isFinite(exponent))) {
      if (base.equals(-0d)) {
        if (NumberUtils.isNegative(exponent)) {
          return NumberUtils.isInteger(exponent) && SCMPredicate.IS_ODD.apply1(exponent) ?
                 Double.NEGATIVE_INFINITY :
                 Double.POSITIVE_INFINITY;
        } else {
          return NumberUtils.isInteger(exponent) && SCMPredicate.IS_ODD.apply1(exponent) ? -0d : 0d;
        }
      }
      return NumberUtils.isNegative(exponent) ?
             Double.valueOf(Double.POSITIVE_INFINITY) :
             NumberUtils.inexactnessTaint(0L, base);
    }
    if (NumberUtils.isOne(base)) {
      return NumberUtils.inexactnessTaint(base, exponent);
    }
    if (NumberUtils.isZero(exponent)) {
      return 1L;
    }
    if (NumberUtils.isOne(exponent)) {
      return base;
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
    if ((base instanceof Double) && Double.NEGATIVE_INFINITY == (Double)base) {
      if (NumberUtils.isInteger(exponent)) {
        if (NumberUtils.isNegative(exponent)) {
          return SCMPredicate.IS_ODD.apply1(exponent) ? -0d : 0d;
        } else {
          return SCMPredicate.IS_ODD.apply1(exponent) ? Double.NEGATIVE_INFINITY : Double.POSITIVE_INFINITY;
        }
      }
    }

    /* (expt +inf.0 w):
     *  w is negative — 0.0
     *  w is positive — +inf.0
     */
    if ((base instanceof Double) && Double.POSITIVE_INFINITY == (Double)base) {
      return NumberUtils.isPositive(exponent) ? Double.POSITIVE_INFINITY : 0d;
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
    if ((exponent instanceof Double) && Double.isInfinite((Double)exponent)) {
      if (base instanceof SCMBigComplex) {
        return Double.NaN;
      }
      if (NumberUtils.isZero(base)) {
        if ((Double) exponent == Double.NEGATIVE_INFINITY) {
          throw new ArithmeticException(String.format("%s: undefined for %s and %s", "expt", base, Writer.write(exponent)));
        } else {
          return 0L;
        }
      }
      if ((Double) exponent == Double.NEGATIVE_INFINITY) {
        if (NumericalComparison.LESS.apply(base, 1L)) {
          return Double.POSITIVE_INFINITY;
        } else if (NumericalComparison.GREATER.apply(base, 1L)) {
          return 0d;
        }
      } else if ((Double) exponent == Double.POSITIVE_INFINITY) {
        if (NumericalComparison.LESS.apply(base, 1L)) {
          return 0d;
        } else if (NumericalComparison.GREATER.apply(base, 1L)) {
          return Double.POSITIVE_INFINITY;
        }
      }
    }

    /* Complex numbers */
    if ((base instanceof SCMBigComplex) || (exponent instanceof SCMBigComplex) ) {
      return SCMBigComplex.of(base).expt(SCMBigComplex.of(exponent));
    }
    /* Long, Integer, Short, Byte */
    Number b = NumberUtils.upcast(base);
    Number ex = NumberUtils.upcast(exponent);
    if ((b instanceof Long) && (ex instanceof Long)) {
      boolean isNegative = false;
      if (exponent.longValue() < Integer.MAX_VALUE) {
        int e = exponent.intValue();
        if (e < 0) {
          isNegative = true;
          e = Math.abs(e);
        }
        BigDecimal result = BigDecimal.valueOf(base.longValue()).pow(e).setScale(0, NumberUtils.ROUNDING_MODE);
        if (isNegative) {
          return new SCMBigRational(BigInteger.ONE, result.toBigInteger());
        }
        return NumberUtils.downcastNumber(result);
      } else {
        /* If we came here, then exponent is greater than Integer.MAX_VALUE */
        if (Math.abs(base.longValue()) < 1) {
          return 0L;
        }
        if (base.longValue() > 0) {
          return Double.POSITIVE_INFINITY;
        } else {
          return SCMPredicate.IS_ODD.apply1(exponent) ? Double.NEGATIVE_INFINITY : Double.POSITIVE_INFINITY;
        }
      }
    }
    /* BigDecimals */
    if (base instanceof BigDecimal && NumberUtils.isInteger(exponent)) {
      if (NumberUtils.isInteger(base)) {
        if (exponent instanceof BigDecimal) {
          try {
            return ((BigDecimal) base).pow(((BigDecimal) exponent).intValueExact());
          } catch (ArithmeticException e) {
            return exptBig((BigDecimal) base, (BigDecimal) exponent);
          }
        }
      }
      return exptBig((BigDecimal) base, NumberUtils.toBigDecimal(exponent));
    }
    /* BigIntegers */
    if (base instanceof BigInteger && NumberUtils.isInteger(exponent)) {
      if (NumberUtils.isInteger(base)) {
        if (exponent instanceof BigInteger) {
          try {
            return ((BigInteger) base).pow(((BigInteger) exponent).intValueExact());
          } catch (ArithmeticException e) {
            // ignore
          }
        }
      }
      return exptBig(NumberUtils.toBigDecimal(base), NumberUtils.toBigDecimal(exponent));
    }
    /* Double */
    double result = Math.pow(base.doubleValue(), exponent.doubleValue());
    if (Double.isInfinite(result)) {
      return new BigDecimal(base.toString()).pow(exponent.intValue());
    }
    if (Double.isNaN(result)) {
      return SCMBigComplex.of(base).expt(SCMBigComplex.of(exponent));
    }
    return result;
  }

  private static Number exptBig(BigDecimal n, BigDecimal e) {
    try {
      int i = e.intValueExact();
      return n.pow(i);
    } catch (ArithmeticException ex) {
      // FIXME NEGATIVE_INFINITY and zero in some cases?
      return Double.POSITIVE_INFINITY;
    }
  }
}
