package core.procedures.math;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;

import java.math.BigDecimal;

public class Max extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "max";
  }

  public Number invoke(Number first, Number second) {
    if ((first instanceof Long) && (second instanceof Long)) {
      return Math.max((Long)first, (Long)second);
    }

    if ((first instanceof BigDecimal) && (second instanceof BigDecimal)) {
      return ((BigDecimal)first).max((BigDecimal) second);
    }
    if (first instanceof BigDecimal) {
      return ((BigDecimal)first).max(new BigDecimal(second.toString()));
    }
    if (second instanceof BigDecimal) {
      return ((BigDecimal)second).max(new BigDecimal(first.toString()));
    }

    return Math.max(first.doubleValue(), second.doubleValue());
  }

  @Override
  public Object invoke(Object... args) {
    if (args != null && args.length > 0) {
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
        result = invoke((Number)first, (Number)args[i]);
      }
      return result;
    }
    throw new ArityException(args.length, 1, getName());
  }
}
