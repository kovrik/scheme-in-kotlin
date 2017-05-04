package core.procedures.delayed;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMDelay;
import core.scm.SCMFuture;
import core.scm.SCMPromise;

public final class Force extends AFn {

  public Force() {
    super(new FnArgsBuilder().min(1).max(1).build());
  }

  @Override
  public String getName() {
    return "force";
  }

  @Override
  public Object apply1(Object arg) {
    if (arg instanceof SCMFuture || arg instanceof SCMPromise) {
      return arg;
    }
    /* Force derefs delays */
    if (arg instanceof SCMDelay) {
      return ((SCMDelay) arg).deref();
    }
    throw new WrongTypeException(getName(), "Delay or Promise or Future", arg);
  }
}
