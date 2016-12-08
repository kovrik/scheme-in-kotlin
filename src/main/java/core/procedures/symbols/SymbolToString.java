package core.procedures.symbols;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMSymbol;

@FnArgs(args = {SCMSymbol.class})
public class SymbolToString extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "symbol->string";
  }

  @Override
  public String invoke(Object... args) {
    return args[0].toString();
  }
}
