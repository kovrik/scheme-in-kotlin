package core.procedures.math.numeric;

import core.procedures.AFn;

import java.math.BigDecimal;

public class Max extends AFn implements INumericalOperation {

  public Number zero() {
    return 1L;
  }

  public Object apply(Object first, Object second) {
    return apply((Number)first, (Number)second);
  }

  public Number apply(Number first, Number second) {
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
    if (args != null) {
      if (args.length == 1) {
        return args[0];
      }
      Object result = args[0];
      if (!(result instanceof Number)) {
        throw new IllegalArgumentException("Wrong argument type. Expected: Number, actual: " + result.getClass().getSimpleName());
      }
      for (int i = 1; i < args.length; i++) {
        Number first = (Number)result;
        if (!(args[i] instanceof Number)) {
          throw new IllegalArgumentException("Wrong argument type. Expected: Number, actual: " + args[i].getClass().getSimpleName());
        }
        Number second = (Number)args[i];
        result = apply(first, second);
      }
      return result;
    }
    return throwArity(1);
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
