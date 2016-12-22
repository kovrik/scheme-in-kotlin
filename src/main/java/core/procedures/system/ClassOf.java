package core.procedures.system;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMClass;

@FnArgs(minArgs = 1, maxArgs = 1)
public class ClassOf extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "class-of";
  }

  @Override
  public SCMClass apply(Object... args) {
    return SCMClass.classOf(args[0]);
  }
}
