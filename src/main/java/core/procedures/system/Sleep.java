package core.procedures.system;

import core.exceptions.ThrowableWrapper;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.Void;

public final class Sleep extends AFn {

  public Sleep() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[] {Long.class}).build());
  }

  @Override
  public String getName() {
    return "sleep";
  }

  @Override
  public Void apply1(Object arg) {
    try {
      Thread.sleep(((Number)arg).longValue());
    } catch (InterruptedException e) {
      throw new ThrowableWrapper(e);
    }
    return Void.VOID;
  }
}
