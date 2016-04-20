package main.functions.math;

import main.functions.IFn;

public class Division implements INumericalOperation, IFn {

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

  public Object invoke(Object... args) {

    if (args == null || args.length == 0) {
      throw new IllegalArgumentException("Wrong number of arguments to -");
    }
    Object result = zero();
    for (Object number : args) {
      result = apply((Number)result, (Number)number);
    }
    return result;
  }

  public Object call() throws Exception {
    return invoke();
  }

  public void run() {
    invoke();
  }
}
