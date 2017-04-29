package core.procedures.continuations;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMClass;

/*
 * "Upward" one-shot continuation
 */
public final class Continuation extends AFn {

  public Continuation() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1));
  }

  private boolean invoked = false;

  public boolean isInvoked() {
    return invoked;
  }

  public void invalidate() {
    this.invoked = true;
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
  public Number apply1(Object arg) {
    throw new CalledContinuation(arg, this);
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
