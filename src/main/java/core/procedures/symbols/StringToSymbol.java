package core.procedures.symbols;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMSymbol;

public class StringToSymbol extends AFn {

  public StringToSymbol() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1).mandatoryArgsTypes(new Class[]{CharSequence.class}));
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
    return SCMSymbol.of(arg.toString());
  }
}
