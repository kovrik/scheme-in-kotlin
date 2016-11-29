package core.procedures.math;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMBigRational;
import core.utils.BigDecimalMath;
import core.utils.NumberUtils;

import java.math.BigDecimal;
import java.math.BigInteger;

public class Expt extends AFn {

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
    if (args != null && args.length == 2) {
      if (!(args[0] instanceof Number)) {
        throw new WrongTypeException("Number", args[0]);
      }
      if (!(args[1] instanceof Number)) {
        throw new WrongTypeException("Number", args[1]);
      }
      return invoke((Number)args[0], (Number)args[1]);
    }
    throw new ArityException(args.length, 2, getName());
  }

  // FIXME Check other special cases: Negative infinity, NaN, zero?
  public static Number invoke(Number first, Number exponent) {
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
    if ((first instanceof BigDecimal) && (exponent instanceof BigDecimal)) {
      return BigDecimalMath.pow((BigDecimal)first, (BigDecimal)exponent);
    }
    if (first instanceof BigDecimal) {
      return BigDecimalMath.pow((BigDecimal)first, new BigDecimal(exponent.toString()));
    }
    if (exponent instanceof BigDecimal) {
      return BigDecimalMath.pow(new BigDecimal(first.toString()), (BigDecimal)exponent);
    }

    double result = Math.pow(first.doubleValue(), exponent.doubleValue());
    if (Double.isInfinite(result)) {
      return new BigDecimal(first.toString()).pow(exponent.intValue());
    }
    return result;
  }
}
