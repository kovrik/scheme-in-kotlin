package core.procedures.continuations;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.Type;

/*
 * "Upward" one-shot continuation
 */
public final class Continuation extends AFn {

  public Continuation() {
    super(new FnArgsBuilder().min(1).max(1).build());
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
  public Type getType() {
    return Type.CONTINUATION;
  }

  @Override
  public String toString() {
    return "#<continuation>";
  }
}
