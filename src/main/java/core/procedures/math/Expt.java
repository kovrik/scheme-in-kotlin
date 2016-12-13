package core.procedures.math;

import core.procedures.AFn;
import core.scm.FnArgs;
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
  public Number invoke(Object... args) {
    return invoke((Number)args[0], (Number)args[1]);
  }

  public static Number invoke(Number first, Number exponent) {
    /* Special cases */
    if (NumberUtils.isZero(first)) {
      return NumberUtils.inexactnessTaint(first, exponent);
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
