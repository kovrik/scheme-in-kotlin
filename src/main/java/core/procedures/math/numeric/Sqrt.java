package core.procedures.math.numeric;

import core.exceptions.ArityException;
import core.procedures.AFn;

import java.math.BigDecimal;

public class Sqrt extends AFn implements INumericalOperation {

  @Override
  public Number invoke(Object... args) {
    if (args != null && args.length == 1) {
      if (args[0] instanceof Long) {
        return Math.sqrt((Long)args[0]);
      } else if (args[0] instanceof Double) {
        return Math.sqrt((Double) args[0]);
      } else if (args[0] instanceof BigDecimal) {
        return Double.POSITIVE_INFINITY;
      }
      throw new IllegalArgumentException("Wrong type argument to `sqrt`");
    }
    throw new ArityException(args.length, 1, "sqrt");
  }

  public Number zero() {
    throw new ArityException(0, 1, "sqrt");
  }

  public Number apply(Number first, Number second) {
    throw new ArityException(2, 1, "sqrt");
  }

  public Object apply(Object first, Object second) {
    throw new ArityException(2, 1, "sqrt");
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
