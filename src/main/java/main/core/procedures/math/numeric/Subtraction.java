package main.core.procedures.math.numeric;

import main.core.exceptions.ArityException;
import main.core.procedures.AFn;

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
    return first.doubleValue() - second.doubleValue();
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
