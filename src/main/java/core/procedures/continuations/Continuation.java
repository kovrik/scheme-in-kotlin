package core.procedures.continuations;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMClass;

@FnArgs(args = {Object.class})
public class Continuation extends AFn {

  private boolean valid = true;

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

  public boolean isValid() {
    return valid;
  }

  public void markInvalid() {
    this.valid = false;
  }
}
