package core.procedures.symbols;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMSymbol;

@FnArgs(args = {String.class})
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
  public SCMSymbol apply(Object... args) {
    return new SCMSymbol(args[0].toString());
  }
}
