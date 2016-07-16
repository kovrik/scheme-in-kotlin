package core.procedures.math.numeric;

import core.exceptions.ArityException;
import core.procedures.AFn;

import java.math.BigDecimal;

public class Subtraction extends AFn implements INumericalOperation {

  public Number zero() {
    return 0L;
  }

  @Override
  public Object invoke(Object... args) {

    if (args == null || args.length == 0) {
      throw new ArityException(0, "-");
    }
    if (args.length == 1) {
      return apply(zero(), (Number)args[0]);
    }
    Object result = args[0];
    for (int i = 1; i < args.length; i++) {
      result = apply((Number)result, (Number)args[i]);
    }
    return result;
  }

  public Object apply(Object first, Object second) {
    return apply((Number)first, (Number)second);
  }

  public Number apply(Number first, Number second) {
    if ((first instanceof Long) && (second instanceof Long)) {
      return (Long)first - (Long)second;
    }
    if (first instanceof BigDecimal) {
      return ((BigDecimal)first).subtract(new BigDecimal(second.toString()));
    }
    if (second instanceof BigDecimal) {
      return ((BigDecimal)second).subtract(new BigDecimal(first.toString()));
    }
    double result = first.doubleValue() - second.doubleValue();
    if (Double.isNaN(result) || Double.isInfinite(result)) {
      return new BigDecimal(first.toString()).subtract(new BigDecimal(second.toString()));
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
