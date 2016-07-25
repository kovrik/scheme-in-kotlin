package core.procedures.symbols;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMSymbol;

public class SymbolToString extends AFn {

  @Override
  public String invoke(Object... args) {
    if (args != null && args.length == 1) {
      if (args[0] instanceof SCMSymbol) {
        return ((SCMSymbol)args[0]).getValue();
      }
      throw new WrongTypeException("Symbol", args[0]);
    }
    throw new ArityException(args.length, 1, "symbol->string");
  }
}
