package main.core.math.bool;

import main.core.procedures.IFn;

public class Negation implements IBooleanOperation, IFn {

  private static final String UNSUPPORTED_OPERATION = "Wrong number of arguments to `" ;//+ Keywords.NOT.getValue() + "`";

  public Object invoke(Object... args) {
    if (args != null && args.length == 1) {
      return (!(Boolean)args[0]);
    }
    throw new UnsupportedOperationException(UNSUPPORTED_OPERATION);
  }

  public Boolean zero() {
    throw new UnsupportedOperationException(UNSUPPORTED_OPERATION);
  }

  public Boolean apply(Boolean first, Boolean second) {
    throw new UnsupportedOperationException(UNSUPPORTED_OPERATION);
  }

  public Object apply(Object first, Object second) {
    throw new UnsupportedOperationException(UNSUPPORTED_OPERATION);
  }

  public Object call() throws Exception {
    return invoke();
  }

  public void run() {
    invoke();
  }
}
