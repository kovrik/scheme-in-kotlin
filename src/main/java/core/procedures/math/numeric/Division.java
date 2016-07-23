package core.procedures.math.numeric;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;

import java.math.BigDecimal;

public class Division extends AFn implements INumericalOperation {

  @Override
  public Number zero() {
    return 1L;
  }

  @Override
  public Number apply(Object numenator, Object denominator) {
    return apply((Number)numenator, (Number)denominator);
  }

  @Override
  public Number apply(Number numenator, Number denominator) {

    if ((numenator instanceof Long) &&
        (denominator instanceof Long) &&
        // FIXME Optimize?
        ((Long)numenator % (Long)denominator) == 0) {

      return (Long)numenator / (Long)denominator;
    }
    if (numenator instanceof BigDecimal) {
      return ((BigDecimal)numenator).divide(new BigDecimal(denominator.toString()));
    }
    if (denominator instanceof BigDecimal) {
      return ((BigDecimal)denominator).divide(new BigDecimal(numenator.toString()));
    }
    double result = numenator.doubleValue() / denominator.doubleValue();
    if (Double.isNaN(result) || Double.isInfinite(result)) {
      return new BigDecimal(numenator.toString()).divide(new BigDecimal(denominator.toString()), BigDecimal.ROUND_HALF_UP);
    }
    return result;
  }

  @Override
  public Number invoke(Object... args) {
    if (args == null || args.length == 0) {
      throw new ArityException(0, "/");
    }
    if (!(args[0] instanceof Number)) {
      throw new WrongTypeException("Number", args[0]);
    }
    Number result;
    if (args.length == 1) {
      return apply(1, args[0]);
    } else {
      result = (Number)args[0];
    }
    for (int d = 1; d <= args.length - 1; d++) {
      if (!(args[d] instanceof Number)) {
        throw new WrongTypeException("Number", args[d]);
      }
      result = apply(result, args[d]);
    }
    return result;
  }
}
