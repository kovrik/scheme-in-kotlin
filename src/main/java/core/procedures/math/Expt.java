package core.procedures.math;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigComplex;
import core.scm.SCMBigRational;
import core.utils.BigDecimalMath;
import core.utils.NumberUtils;

import java.math.BigDecimal;
import java.math.BigInteger;

@FnArgs(args = {Number.class, Number.class})
public class Expt extends AFn {

  /* Return 0d if exponent is less than -745 */
  private static final BigDecimal NEG_LIMIT = new BigDecimal("-745");

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "expt";
  }

  @Override
  public Number apply(Object... args) {
    return expt((Number)args[0], (Number)args[1]);
  }

  /**
   * Special cases when w is a real number:
   * These special cases correspond to pow in C99 [C99], except when z is negative and w is a not an integer.
   * See: https://docs.racket-lang.org/reference/generic-numbers.html#(def._((quote._~23~25kernel)._expt))
   *
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
   *
   * (expt z -inf.0) for positive z:
   *
   *   z is less than 1.0 — +inf.0
   *   z is greater than 1.0 — 0.0
   *
   * (expt z +inf.0) for positive z:
   *
   *  z is less than 1.0 — 0.0
   *  z is greater than 1.0 — +inf.0
   *
   * (expt -inf.0 w) for integer w:
   *  w is negative:
   *  w is odd  — -0.0
   *  w is even —  0.0
   *
   *  w is positive:
   *   w is odd  — -inf.0
   *   w is even — +inf.0
   *
   * (expt +inf.0 w):
   *  w is negative — 0.0
   *  w is positive — +inf.0
   */
  public static Number expt(Number first, Number exponent) {
    /* Special cases */
    if (NumberUtils.isZero(first)) {
      if (NumberUtils.isNegative(exponent)) {
        return Double.POSITIVE_INFINITY;
      } else if (NumberUtils.isPositive(exponent)) {
        return NumberUtils.inexactnessTaint(first, exponent);
      } else if (NumberUtils.isZero(exponent)) {
        return 1L;
      }
    }
    if (NumberUtils.isOne(first)) {
      return NumberUtils.inexactnessTaint(first, exponent);
    }
    if (NumberUtils.isZero(exponent)) {
      return 1L;
    }
    if (NumberUtils.isOne(exponent)) {
      return first;
    }
    /* Complex numbers */
    if ((first instanceof SCMBigComplex) || (exponent instanceof SCMBigComplex) ) {
      return SCMBigComplex.of(first).expt(SCMBigComplex.of(exponent));
    }
    /* Special cases for Real numbers */
    if ((first instanceof Double) && Double.NEGATIVE_INFINITY == (Double)first) {
      if (NumberUtils.isInteger(exponent)) {
        if (NumberUtils.isNegative(exponent)) {
          // TODO check if exponent is odd or even
          return 0d;
        } else {
          // TODO check if exponent is odd or even
          return Double.NEGATIVE_INFINITY;
        }
      }
    }
    if ((first instanceof Double) && Double.POSITIVE_INFINITY == (Double)first) {
      if (NumberUtils.isPositive(exponent)) {
        return Double.POSITIVE_INFINITY;
      } else {
        return 0d;
      }
    }

    /* FIXME probably wrong BEGIN ------------------------------- */
    if ((first instanceof Long) || (exponent instanceof Long)) {
      int scale = 0;
      if (exponent instanceof Double) {
        scale = 1;
      } else if (exponent instanceof BigDecimal) {
        scale = ((BigDecimal)exponent).scale();
      }
      boolean isNegative = false;
      int e = exponent.intValue();
      if (exponent.intValue() < 0) {
        isNegative = true;
        e = Math.abs(exponent.intValue());
      }
      BigDecimal result = new BigDecimal(first.toString()).pow(e).setScale(scale, NumberUtils.ROUNDING_MODE);
      if (isNegative) {
        return new SCMBigRational(BigInteger.ONE, result.toBigInteger());
      }
      return result;
    }
    /* FIXME probably wrong END --------------------------------- */
    /* BigDecimals */
    try {
      if ((first instanceof BigDecimal) && (exponent instanceof BigDecimal)) {
        return BigDecimalMath.pow((BigDecimal) first, (BigDecimal) exponent);
      }
      if (first instanceof BigDecimal) {
        if (exponent instanceof SCMBigRational) {
          // FIXME Check rounding mode and precision
          return BigDecimalMath.pow((BigDecimal) first,
                                    ((SCMBigRational)exponent).toBigDecimal().setScale(NumberUtils.DEFAULT_SCALE, NumberUtils.ROUNDING_MODE));
        }
        return BigDecimalMath.pow((BigDecimal) first, new BigDecimal(exponent.toString()));
      }
      if (exponent instanceof BigDecimal) {
        if (first instanceof SCMBigRational) {
          // FIXME Check rounding mode and precision
          return BigDecimalMath.pow(((SCMBigRational)first).toBigDecimal().setScale(NumberUtils.DEFAULT_SCALE, NumberUtils.ROUNDING_MODE),
                                    new BigDecimal(exponent.toString()));
        }
        return BigDecimalMath.pow(new BigDecimal(first.toString()), (BigDecimal) exponent);
      }
    } catch (ArithmeticException e) {
      BigDecimal exp;
      if (exponent instanceof BigDecimal) {
        exp = (BigDecimal)exponent;
      } else {
        exp = new BigDecimal(exponent.toString());
      }
      if (exp.compareTo(NEG_LIMIT) < 0) {
        return 0d;
      } else {
        return Double.POSITIVE_INFINITY;
      }
    }

    double result = Math.pow(first.doubleValue(), exponent.doubleValue());
    if (Double.isInfinite(result)) {
      return new BigDecimal(first.toString()).pow(exponent.intValue());
    }
    return result;
  }
}
