package core.procedures.symbols;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMSymbol;

@FnArgs(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = {String.class})
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
  public SCMSymbol apply1(Object arg) {
    return new SCMSymbol(arg.toString());
  }
}
