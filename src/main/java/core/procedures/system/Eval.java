package core.procedures.system;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMTailCall;

@FnArgs(args = {Object.class})
public class Eval extends AFn {

  @Override
  public boolean isPure() {
    return false;
  }

  @Override
  public String getName() {
    return "eval";
  }

  @Override
  public Object apply(Object... args) {
    return new SCMTailCall(args[0], null);
  }
}
