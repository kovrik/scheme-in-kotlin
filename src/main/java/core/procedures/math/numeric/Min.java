package core.procedures.math.numeric;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;

import java.math.BigDecimal;

public class Min extends AFn implements INumericalOperation {

  @Override
  public Number zero() {
    return 1L;
  }

  @Override
  public Object apply(Object first, Object second) {
    return apply((Number)first, (Number)second);
  }

  @Override
  public Number apply(Number first, Number second) {
    if ((first instanceof Long) && (second instanceof Long)) {
      return Math.min((Long)first, (Long)second);
    }
    if ((first instanceof BigDecimal) && (second instanceof BigDecimal)) {
      return ((BigDecimal)first).min((BigDecimal) second);
    }
    if (first instanceof BigDecimal) {
      return ((BigDecimal)first).min(new BigDecimal(second.toString()));
    }
    if (second instanceof BigDecimal) {
      return ((BigDecimal)second).min(new BigDecimal(first.toString()));
    }

    return Math.min(first.doubleValue(), second.doubleValue());
  }

  @Override
  public Object invoke(Object... args) {
    if (args != null) {
      if (args.length == 1) {
        return args[0];
      }
      Object result = args[0];
      if (!(result instanceof Number)) {
        throw new WrongTypeException("Number", result);
      }
      for (int i = 1; i < args.length; i++) {
        Number first = (Number)result;
        if (!(args[i] instanceof Number)) {
          throw new WrongTypeException("Number", args[i]);
        }
        Number second = (Number)args[i];
        result = apply(first, second);
      }
      return result;
    }
    return throwArity(1);
  }
}
