package core.procedures.system;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.specialforms.TailCall;

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
  public Object invoke(Object... args) {
    return new TailCall(args[0], null);
  }
}
