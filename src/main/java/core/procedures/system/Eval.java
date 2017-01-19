package core.procedures.system;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMThunk;

@FnArgs(minArgs = 1, maxArgs = 1)
public final class Eval extends AFn {

  @Override
  public boolean isPure() {
    return false;
  }

  @Override
  public String getName() {
    return "eval";
  }

  @Override
  public Object apply1(Object arg) {
    return new SCMThunk(arg, null);
  }
}
