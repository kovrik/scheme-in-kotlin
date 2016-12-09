package core.procedures.system;

import core.procedures.AFn;
import core.scm.FnArgs;

@FnArgs(args = {Object.class})
public class ToString extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "->string";
  }

  @Override
  public Object invoke(Object... args) {
    return args[0].toString();
  }
}
