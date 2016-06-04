package core.procedures.math.numeric;

import core.exceptions.ArityException;
import core.procedures.AFn;

public class Abs extends AFn implements INumericalOperation {

  @Override
  public Number invoke(Object... args) {
    if (args != null && args.length == 1) {
      if (args[0] instanceof Long) {
        return Math.abs((Long)args[0]);
      } else if (args[0] instanceof Double) {
        return Math.abs((Double) args[0]);
      }
      throw new IllegalArgumentException("Wrong type argument to `abs`");
    }
    throw new ArityException(args.length, "abs");
  }

  public Number zero() {
    throw new ArityException(0, "abs");
  }

  public Number apply(Number first, Number second) {
    throw new ArityException(2, "abs");
  }

  public Object apply(Object first, Object second) {
    throw new ArityException(2, "abs");
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
