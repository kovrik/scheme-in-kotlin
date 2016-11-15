package core.procedures.symbols;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMSymbol;

public class StringToSymbol extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "string->symbol";
  }

  @Override
  public SCMSymbol invoke(Object... args) {
    if (args != null && args.length == 1) {
      if (args[0] instanceof String) {
        return new SCMSymbol((String)args[0]);
      }
      throw new WrongTypeException("String", args[0]);
    }
    throw new ArityException(args.length, 1, getName());
  }
}
