package core.procedures.system;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

public final class Identity extends AFn {

  public Identity() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1));
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "identity";
  }

  @Override
  public Object apply1(Object arg) {
    return arg;
  }
}
