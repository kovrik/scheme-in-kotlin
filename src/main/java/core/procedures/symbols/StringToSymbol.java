package core.procedures.symbols;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMSymbol;

public class StringToSymbol extends AFn {

  @Override
  public SCMSymbol invoke(Object... args) {
    if (args != null && args.length == 1) {
      if (args[0] instanceof String) {
        return new SCMSymbol((String)args[0]);
      }
      throw new WrongTypeException("String", args[0]);
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
}
