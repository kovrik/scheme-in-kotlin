package core.procedures.symbols;

import core.exceptions.ArityException;
import core.procedures.AFn;
import core.scm.SCMSymbol;

public class StringToSymbol extends AFn {

  @Override
  public SCMSymbol invoke(Object... args) {
    if (args != null && args.length == 1) {
      if (args[0] instanceof String) {
        return new SCMSymbol((String)args[0]);
      }
      throw new IllegalArgumentException("Wrong argument type. Expected: String, actual: " + args[0].getClass().getSimpleName());
    }
    throw new ArityException(args.length, 1, "string->symbol");
  }

  public Number zero() {
    throw new ArityException(0, 1, "string->symbol");
  }

  public Number apply(Number first, Number second) {
    throw new ArityException(2, 1, "string->symbol");
  }

  public Object apply(Object first, Object second) {
    throw new ArityException(2, 1, "string->symbol");
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
