package main.core.procedures.math.numeric;

import main.core.exceptions.ArityException;
import main.core.procedures.AFn;

public class Division extends AFn implements INumericalOperation {

  public Number zero() {
    return 1L;
  }

  public Object apply(Object first, Object second) {
    return apply((Number)first, (Number)second);
  }

  public Number apply(Number first, Number second) {

    if ((first instanceof Long) && (second instanceof Long)) {
      return (Long)first / (Long)second;
    }
    return first.doubleValue() / second.doubleValue();
  }

  @Override
  public Object invoke(Object... args) {

    if (args == null || args.length == 0) {
      throw new ArityException(0, "\\");
    }
    Object result = zero();
    for (Object number : args) {
      result = apply((Number)result, (Number)number);
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
