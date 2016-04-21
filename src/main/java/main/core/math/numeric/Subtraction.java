package main.core.math.numeric;

import main.core.procedures.IFn;

public class Subtraction implements INumericalOperation, IFn {

  public Number zero() {
    return 0L;
  }

  public Object invoke(Object... args) {

    if (args == null || args.length == 0) {
      throw new IllegalArgumentException("Wrong number of arguments to -");
    }
    if (args.length == 1) {
      return apply(0L, (Number)args[0]);
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

  public Object call() throws Exception {
    return invoke();
  }

  public void run() {
    invoke();
  }
}
