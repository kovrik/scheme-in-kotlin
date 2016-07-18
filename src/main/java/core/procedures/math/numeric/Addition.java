package core.procedures.math.numeric;

import core.procedures.AFn;
import core.writer.Writer;

import java.math.BigDecimal;

public class Addition extends AFn implements INumericalOperation {

  @Override
  public Number zero() {
    return 0L;
  }

  @Override
  public Object apply(Object first, Object second) {
    return apply((Number)first, (Number)second);
  }

  @Override
  public Number apply(Number first, Number second) {
    if ((first instanceof Long) && (second instanceof Long)) {
      return (Long)first + (Long)second;
    }
    if (first instanceof BigDecimal) {
      return ((BigDecimal)first).add(new BigDecimal(second.toString()));
    }
    if (second instanceof BigDecimal) {
      return ((BigDecimal)second).add(new BigDecimal(first.toString()));
    }
    double result = first.doubleValue() + second.doubleValue();
    if (Double.isNaN(result) || Double.isInfinite(result)) {
      return new BigDecimal(first.toString()).add(new BigDecimal(second.toString()));
    }
    return result;
  }

  @Override
  public Object invoke(Object... args) {
    Object result = zero();
    if (args != null) {
      for (Object obj : args) {
        if (!(obj instanceof Number)) {
          throw new IllegalArgumentException("Wrong argument type. Expected: Number, actual: " + Writer.write(obj));
        }
        result = apply((Number) result, (Number) obj);
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
