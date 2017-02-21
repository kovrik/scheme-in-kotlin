package core.procedures.math;

import core.procedures.AFn;
import core.procedures.predicates.SCMPredicate;
import core.scm.FnArgs;
import core.scm.SCMBigComplex;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;
import core.writer.Writer;

import java.math.BigDecimal;
import java.math.BigInteger;

@FnArgs(minArgs = 2, maxArgs = 2, mandatoryArgsTypes = {Number.class, Number.class})
public final class Expt extends AFn {

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
   *
   * Special cases when w is a real number:
   * These special cases correspond to pow in C99 [C99], except when z is negative and w is a not an integer.
   * See: https://docs.racket-lang.org/reference/generic-numbers.html#(def._((quote._~23~25kernel)._expt))
   */
  public static Number expt(Number base, Number exponent) {
    /* Special cases */
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
    if (NumberUtils.isZero(base) && NumberUtils.isZero(exponent)) {
      return 1L;
    }
    if (NumberUtils.isZero(base) && (NumberUtils.isFinite(exponent))) {
      if (base.equals(-0d)) {
        if (NumberUtils.isNegative(exponent)) {
          if (NumberUtils.isInteger(exponent) && SCMPredicate.IS_ODD.apply1(exponent)) {
            return Double.NEGATIVE_INFINITY;
          }
          return Double.POSITIVE_INFINITY;
        } else {
          if (NumberUtils.isInteger(exponent) && SCMPredicate.IS_ODD.apply1(exponent)) {
            return -0d;
          } else {
            return 0d;
          }
        }
      }
      if (NumberUtils.isNegative(exponent)) {
        return Double.POSITIVE_INFINITY;
      } else {
        return NumberUtils.inexactnessTaint(0L, base);
      }
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
          if (SCMPredicate.IS_ODD.apply1(exponent)) {
            return -0d;
          } else {
            return 0d;
          }
        } else {
          if (SCMPredicate.IS_ODD.apply1(exponent)) {
            return Double.NEGATIVE_INFINITY;
          } else {
            return Double.POSITIVE_INFINITY;
          }
        }
      }
    }

    /* (expt +inf.0 w):
     *  w is negative — 0.0
     *  w is positive — +inf.0
     */
    if ((base instanceof Double) && Double.POSITIVE_INFINITY == (Double)base) {
      if (NumberUtils.isPositive(exponent)) {
        return Double.POSITIVE_INFINITY;
      } else {
        return 0d;
      }
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

    // FIXME This code is wrong and probably doesn't work in many cases BEGIN ---------------------------------------------------
    if ((base instanceof Long) && (exponent instanceof Long)) {
      boolean isNegative = false;
      if (exponent.longValue() < Integer.MAX_VALUE) {
        int e = exponent.intValue();
        if (e < 0) {
          isNegative = true;
          e = Math.abs(e);
        }
        BigDecimal result = new BigDecimal(base.toString()).pow(e).setScale(0, NumberUtils.ROUNDING_MODE);
        if (isNegative) {
          return new SCMBigRational(BigInteger.ONE, result.toBigInteger());
        }
        return result;
      } else {
        /* If we came here, then exponent is greater than Integer.MAX_VALUE */
        if (Math.abs(base.longValue()) < 1) {
          return 0L;
        }
        if (base.longValue() > 0) {
          return Double.POSITIVE_INFINITY;
        } else {
          return Double.NEGATIVE_INFINITY;
        }
      }
    }
    /* BigDecimals */
    if (base instanceof BigDecimal && NumberUtils.isInteger(exponent)) {
      if (NumberUtils.isInteger(base)) {
        int exp;
        try {
          if (exponent instanceof BigDecimal) {
            exp = ((BigDecimal) exponent).intValueExact();
          } else {
            exp = exponent.intValue();
          }
          return ((BigDecimal) base).pow(exp);
        } catch (ArithmeticException e) {
          return exptBig((BigDecimal)base, new BigDecimal(exponent.toString()));
        }
      } else {
        return exptBig((BigDecimal) base, new BigDecimal(exponent.toString()));
      }
    }
    double result = Math.pow(base.doubleValue(), exponent.doubleValue());
    if (Double.isInfinite(result)) {
      return new BigDecimal(base.toString()).pow(exponent.intValue());
    }
    return result;
    // FIXME This code is wrong and probably doesn't work in many cases END -----------------------------------------------------
  }

  private static Number exptBig(BigDecimal n, BigDecimal e) {
    try {
      int i = e.intValueExact();
      return n.pow(i);
    } catch (ArithmeticException ex) {
      // FIXME NEGATIVE_INFINITY and zero in some cases
      return Double.POSITIVE_INFINITY;
    }
  }
}
