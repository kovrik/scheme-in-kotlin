package core.procedures.math;

import core.scm.FnArgs;
import core.scm.SCMBoolean;
import core.procedures.AFn;

@FnArgs(minArgs = 1, maxArgs = 1)
public final class Negation extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "not";
  }

  @Override
  public Boolean apply1(Object arg) {
    return !SCMBoolean.toBoolean(arg);
  }
}
