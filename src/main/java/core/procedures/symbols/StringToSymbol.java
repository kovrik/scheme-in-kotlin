package core.procedures.symbols;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMSymbol;

public class StringToSymbol extends AFn {

  public StringToSymbol() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{CharSequence.class}).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "string->symbol";
  }

  @Override
  public SCMSymbol apply1(Object arg) {
    return SCMSymbol.intern(arg.toString());
  }
}
