package core.procedures.math;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.io.Load;

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
  // FIXME Negative exponent
  public Number invoke(Number first, Number second) {
    if ((first instanceof Long) || (second instanceof Long)) {
      int scale = 0;
      if (second instanceof Double) {
        scale = 1;
      } else if (second instanceof BigDecimal) {
        scale = ((BigDecimal)second).scale();
      }
      return new BigDecimal(first.toString()).pow(second.intValue()).setScale(scale);
    }
    if ((first instanceof BigDecimal) || (second instanceof BigDecimal)) {
      BigDecimal s;
      if (second instanceof BigDecimal) {
        s = (BigDecimal)second;
      } else {
        s = new BigDecimal(second.toString());
      }
      if (s.stripTrailingZeros().scale() != 0) {
        return Double.POSITIVE_INFINITY;
      }
      return new BigDecimal(first.toString()).pow(second.intValue());
    }
    double result = Math.pow(first.doubleValue(), second.doubleValue());
    if (Double.isInfinite(result)) {
      return new BigDecimal(first.toString()).pow(second.intValue());
    }
    return result;
  }
}
