package core.procedures.math;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;

import java.math.BigDecimal;

public class Subtraction extends AFn {

  @Override
  public String getName() {
    return "-";
  }

  @Override
  public Object invoke(Object... args) {

    if (args == null || args.length == 0) {
      throw new ArityException(0, "-");
    }
    if (args.length == 1) {
      if (args[0] instanceof BigDecimal) {
        return ((BigDecimal)args[0]).negate();
      }
      return invoke(0L, args[0]);
    }
    Object result = args[0];
    for (int i = 1; i < args.length; i++) {
      if (!(args[0] instanceof Number)) {
        throw new WrongTypeException("Number", args[0]);
      }
      result = invoke((Number)result, (Number)args[i]);
    }
    return result;
  }

  public Number invoke(Number first, Number second) {
    if ((first instanceof Long) && (second instanceof Long)) {
      try {
        return Math.subtractExact((Long)first, (Long)second);
      } catch (ArithmeticException e) {
        return new BigDecimal(first.toString()).subtract(new BigDecimal(second.toString()));
      }
    }
    if (first instanceof BigDecimal) {
      return ((BigDecimal)first).subtract(new BigDecimal(second.toString()));
    }
    if (second instanceof BigDecimal) {
      return new BigDecimal(first.toString()).subtract((BigDecimal)second);
    }
    double result = first.doubleValue() - second.doubleValue();
    if (Double.isNaN(result) || Double.isInfinite(result)) {
      return new BigDecimal(first.toString()).subtract(new BigDecimal(second.toString()));
    }
    return result;
  }
}
