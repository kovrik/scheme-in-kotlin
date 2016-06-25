package core.procedures.symbols;

import core.exceptions.ArityException;
import core.procedures.AFn;
import core.scm.SCMSymbol;

public class SymbolToString extends AFn {

  @Override
  public String invoke(Object... args) {
    if (args != null && args.length == 1) {
      if (args[0] instanceof SCMSymbol) {
        return ((SCMSymbol)args[0]).getValue();
      }
      throw new IllegalArgumentException("Wrong argument type. Expected: Symbol, actual: " + args[0].getClass().getSimpleName());
    }
    throw new ArityException(args.length, 1, "symbol->string");
  }

  public Number zero() {
    throw new ArityException(0, 1, "symbol->string");
  }

  public Number apply(Number first, Number second) {
    throw new ArityException(2, 1, "symbol->string");
  }

  public Object apply(Object first, Object second) {
    throw new ArityException(2, 1, "symbol->string");
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
