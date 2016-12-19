package core.procedures.continuations;

import core.procedures.AFn;
import core.procedures.IFn;
import core.scm.FnArgs;

@FnArgs(args = {IFn.class})
public class CallCC extends AFn {

  @Override
  public boolean isPure() {
    return false;
  }

  @Override
  public String getName() {
    return "call/cc";
  }

  @Override
  public Object apply(Object... args) {
    return new Continuation((IFn) args[0]);
  }
}
