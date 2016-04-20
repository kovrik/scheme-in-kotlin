package main.functions.math;

import main.functions.IFn;

public class Addition implements INumericalOperation, IFn {

  public Number zero() {
    return 0L;
  }

  public Object apply(Object first, Object second) {
    return apply((Number)first, (Number)second);
  }

  public Number apply(Number first, Number second) {

    if ((first instanceof Long) && (second instanceof Long)) {
      return (Long)first + (Long)second;
    }
    return first.doubleValue() + second.doubleValue();
  }

  public Object invoke(Object... args) {

    Object result = zero();
    if (args != null) {
      for (Object number : args) {
        result = apply((Number) result, (Number) number);
      }
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
