package core.procedures.continuations;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMClass;

@FnArgs(minArgs = 1, maxArgs = 1)
public class Continuation extends AFn {

  private boolean valid = true;

  public boolean isValid() {
    return valid;
  }

  public void invalidate() {
    this.valid = false;
  }

  @Override
  public boolean isPure() {
    return false;
  }

  @Override
  public String getName() {
    return "continuation";
  }

  @Override
  public Number apply(Object... args) {
    throw new CalledContinuation(args[0], this);
  }

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.CONTINUATION;
  }

  @Override
  public String toString() {
    return "#<continuation>";
  }
}
