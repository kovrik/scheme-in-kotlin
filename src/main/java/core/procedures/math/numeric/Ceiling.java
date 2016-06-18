package core.procedures.math.numeric;

import core.exceptions.ArityException;
import core.procedures.AFn;

public class Ceiling extends AFn implements INumericalOperation {

  @Override
  public Number invoke(Object... args) {
    if (args != null && args.length == 1) {
      if (args[0] instanceof Long) {
        return (Long)args[0];
      } else if (args[0] instanceof Double) {
        return Math.ceil((Double)args[0]);
      }
      throw new IllegalArgumentException("Wrong argument type. Expected: Number, actual: " + args[0].getClass().getSimpleName());
    }
    throw new ArityException(args.length, 1, "ceiling");
  }

  public Number zero() {
    throw new ArityException(0, 1, "ceiling");
  }

  public Number apply(Number first, Number second) {
    throw new ArityException(2, 1, "ceiling");
  }

  public Object apply(Object first, Object second) {
    throw new ArityException(2, 1, "ceiling");
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
