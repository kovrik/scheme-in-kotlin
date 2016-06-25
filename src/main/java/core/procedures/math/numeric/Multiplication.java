package core.procedures.math.numeric;

import core.procedures.AFn;

import java.math.BigDecimal;

public class Multiplication extends AFn implements INumericalOperation {

  public Number zero() {
    return 1L;
  }

  public Object apply(Object first, Object second) {
    return apply((Number)first, (Number)second);
  }

  public Number apply(Number first, Number second) {
    if ((first instanceof Long) && (second instanceof Long)) {
      return (Long)first * (Long)second;
    }
    double result = first.doubleValue() * second.doubleValue();
    if (Double.isNaN(result) || Double.isInfinite(result)) {
      return new BigDecimal(first.toString()).multiply(new BigDecimal(second.toString()));
    }
    return result;
  }

  @Override
  public Object invoke(Object... args) {
    Object result = zero();
    if (args != null) {
      for (Object number : args) {
        result = apply((Number) result, (Number) number);
      }
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
