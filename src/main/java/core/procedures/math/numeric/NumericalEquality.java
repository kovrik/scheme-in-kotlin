package core.procedures.math.numeric;

import core.procedures.AFn;
import core.procedures.math.IOperation;

public class NumericalEquality extends AFn implements IOperation {

  @Override
  public Boolean invoke(Object... args) {
    Boolean result = zero();
    if (args != null && args.length > 1) {
      for (int i = 0; i < args.length - 1; i++) {
        result = result && apply(args[i], args[i + 1]);
      }
    }
    return result;
  }

  public Boolean zero() {
    return Boolean.TRUE;
  }

  public Boolean apply(Object first, Object second) {

    return ((Number)first).equals((Number)second);
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
