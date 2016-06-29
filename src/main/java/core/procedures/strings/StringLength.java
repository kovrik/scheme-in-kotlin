package core.procedures.strings;

import core.exceptions.ArityException;
import core.procedures.AFn;

public class StringLength extends AFn {

  @Override
  public Long invoke(Object... args) {
    if (args != null && args.length == 1) {
      if (args[0] instanceof String) {
        return ((Integer)((String)args[0]).length()).longValue();
      }
      throw new IllegalArgumentException("Wrong argument type. Expected: String, actual: " + args[0].getClass().getSimpleName());
    }
    throw new ArityException(args.length, 1, "string-length");
  }

  public Number zero() {
    throw new ArityException(0, 1, "string-length");
  }

  public Number apply(Number first, Number second) {
    throw new ArityException(2, 1, "string-length");
  }

  public Object apply(Object first, Object second) {
    throw new ArityException(2, 1, "string-length");
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
