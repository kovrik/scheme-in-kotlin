package core.procedures.system;

import core.exceptions.ThrowableWrapper;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMVoid;

public final class Sleep extends AFn {

  public Sleep() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1).mandatoryArgsTypes(new Class[] {Long.class}));
  }

  @Override
  public String getName() {
    return "sleep";
  }

  @Override
  public SCMVoid apply1(Object arg) {
    try {
      Thread.sleep(((Number)arg).longValue());
    } catch (InterruptedException e) {
      throw new ThrowableWrapper(e);
    }
    return SCMVoid.VOID;
  }
}
