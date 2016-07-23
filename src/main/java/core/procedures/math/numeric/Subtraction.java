package core.procedures.math.numeric;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;

import java.math.BigDecimal;

public class Subtraction extends AFn implements INumericalOperation {

  @Override
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
      if (!(args[0] instanceof Number)) {
        throw new WrongTypeException("Number", args[0]);
      }
      result = apply((Number)result, (Number)args[i]);
    }
    return result;
  }

  @Override
  public Object apply(Object first, Object second) {
    return apply((Number)first, (Number)second);
  }

  @Override
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
}
