package core.procedures.system;

import core.exceptions.ArityException;
import core.procedures.AFn;
import core.scm.specialforms.TailCall;

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
    if (args.length != 1) {
      throw new ArityException(args.length, 1, getName());
    }
    // TODO Check if works as expected
    return new TailCall(args[0], null);
  }
}
