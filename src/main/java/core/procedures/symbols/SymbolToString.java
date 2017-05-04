package core.procedures.symbols;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.Symbol;

public final class SymbolToString extends AFn {

  public SymbolToString() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{Symbol.class}).build());
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
