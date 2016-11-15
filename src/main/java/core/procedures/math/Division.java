package core.procedures.math;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;

import java.math.BigDecimal;

public class Division extends AFn {

  private static final int DEFAULT_ROUNDING_MODE = BigDecimal.ROUND_HALF_UP;

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "/";
  }

  @Override
  public Number invoke(Object... args) {
    if (args == null || args.length == 0) {
      throw new ArityException(0, getName());
    }
    if (!(args[0] instanceof Number)) {
      throw new WrongTypeException("Number", args[0]);
    }
    Number result;
    if (args.length == 1) {
      return invoke((Number)1L, (Number)args[0]);
    } else {
      result = (Number)args[0];
    }
    for (int d = 1; d <= args.length - 1; d++) {
      if (!(args[d] instanceof Number)) {
        throw new WrongTypeException("Number", args[d]);
      }
      result = invoke((Number)result, (Number)args[d]);
    }
    return result;
  }

  public Number invoke(Number numenator, Number denominator) {
    if ((numenator instanceof Long) &&
        (denominator instanceof Long) &&
        ((Long)numenator % (Long)denominator) == 0) {

      return (Long)numenator / (Long)denominator;
    }
    if (numenator instanceof BigDecimal) {
      return ((BigDecimal)numenator).divide(new BigDecimal(denominator.toString()), DEFAULT_ROUNDING_MODE);
    }
    if (denominator instanceof BigDecimal) {
      return new BigDecimal(numenator.toString()).divide(new BigDecimal(denominator.toString()), DEFAULT_ROUNDING_MODE);
    }
    double result = numenator.doubleValue() / denominator.doubleValue();
    if (Double.isNaN(result) || Double.isInfinite(result)) {
      return new BigDecimal(numenator.toString()).divide(new BigDecimal(denominator.toString()), DEFAULT_ROUNDING_MODE);
    }
    return result;
  }
}
