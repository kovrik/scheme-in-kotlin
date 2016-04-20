package main.functions.equality;

import main.functions.IFn;
import main.functions.math.IOperation;

public class Eqv implements IOperation, IFn {

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

    if (first instanceof Character && second instanceof Character) {
      return ((Character) first).equals((Character)second);
    } else if (first instanceof Number && second instanceof Number) {
      return ((Number) first).equals((Number)second);
    }
    return first == second;
  }

  public Object call() throws Exception {
    return invoke();
  }

  public void run() {
    invoke();
  }
}
