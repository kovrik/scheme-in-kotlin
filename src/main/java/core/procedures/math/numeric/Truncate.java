package core.procedures.math.numeric;

import core.exceptions.ArityException;
import core.procedures.AFn;

public class Truncate extends AFn implements INumericalOperation {

  @Override
  public Number invoke(Object... args) {
    if (args != null && args.length == 1) {
      if (args[0] instanceof Long) {
        return (Long)args[0];
      } else if (args[0] instanceof Double) {
        Double arg = (Double) args[0];
        if (arg < 0) {
          return Math.ceil(arg);
        } else {
          return Math.floor(arg);
        }
      }
      throw new IllegalArgumentException("Wrong argument type. Expected: Number, actual: " + args[0].getClass().getSimpleName());
    }
    throw new ArityException(args.length, 1, "truncate");
  }

  public Number zero() {
    throw new ArityException(0, 1, "truncate");
  }

  public Number apply(Number first, Number second) {
    throw new ArityException(2, 1, "truncate");
  }

  public Object apply(Object first, Object second) {
    throw new ArityException(2, 1, "truncate");
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
