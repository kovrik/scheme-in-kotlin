package core.procedures.system;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMThunk;

public final class Eval extends AFn {

  public Eval() {
    super(new FnArgsBuilder().min(1).max(1).build());
  }

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
    return new SCMThunk(arg);
  }
}
