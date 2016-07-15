package core.procedures.math.numeric;

import core.exceptions.ArityException;
import core.procedures.AFn;

import java.math.BigDecimal;
import java.math.BigInteger;

public class Division extends AFn implements INumericalOperation {

  public Number zero() {
    return 1L;
  }

  public Number apply(Object numenator, Object denominator) {
    return apply((Number)numenator, (Number)denominator);
  }

  public Number apply(Number numenator, Number denominator) {

    if ((numenator instanceof Long) &&
        (denominator instanceof Long) &&
        // FIXME Optimize?
        ((Long)numenator % (Long)denominator) == 0) {

      return (Long)numenator / (Long)denominator;
    }
    if (numenator instanceof BigInteger) {
      return ((BigInteger)numenator).divide(new BigInteger(denominator.toString()));
    }
    if (denominator instanceof BigInteger) {
      return ((BigInteger)denominator).divide(new BigInteger(numenator.toString()));
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
    Number result;
    if (args.length == 1) {
      return apply(1, args[0]);
    } else {
      result = (Number)args[0];
    }
    for (int d = 1; d <= args.length - 1; d++) {
      result = apply(result, args[d]);
    }
    return result;
  }

  @Override
  public Object call() throws Exception {
    return invoke();
  }

  @Override
  public void run() {
    invoke();
  }
}
