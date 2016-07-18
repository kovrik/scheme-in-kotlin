package core.procedures.math.numeric;

import core.exceptions.ArityException;
import core.procedures.AFn;

import java.math.BigDecimal;

public class Floor extends AFn implements INumericalOperation {

  @Override
  public Number invoke(Object... args) {
    if (args != null && args.length == 1) {
      if (args[0] instanceof Long) {
        return (Long)args[0];
      } else if (args[0] instanceof Double) {
        return Math.floor((Double)args[0]);
      } else if (args[0] instanceof BigDecimal) {
        BigDecimal arg = (BigDecimal)args[0];
        return arg.setScale(0, BigDecimal.ROUND_DOWN);
      }
      throw new IllegalArgumentException("Wrong argument type. Expected: Number, actual: " + args[0].getClass().getSimpleName());
    }
    throw new ArityException(args.length, 1, "floor");
  }

  @Override
  public Number zero() {
    throw new ArityException(0, 1, "floor");
  }

  @Override
  public Number apply(Number first, Number second) {
    throw new ArityException(2, 1, "floor");
  }

  @Override
  public Object apply(Object first, Object second) {
    throw new ArityException(2, 1, "floor");
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
