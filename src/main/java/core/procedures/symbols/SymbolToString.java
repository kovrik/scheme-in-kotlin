package core.procedures.symbols;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMSymbol;

public final class SymbolToString extends AFn {

  public SymbolToString() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1).mandatoryArgsTypes(new Class[]{SCMSymbol.class}));
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "symbol->string";
  }

  @Override
  public String apply1(Object arg) {
    return arg.toString();
  }
}
