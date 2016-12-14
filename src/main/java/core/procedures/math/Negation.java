package core.procedures.math;

import core.scm.FnArgs;
import core.scm.SCMBoolean;
import core.procedures.AFn;

@FnArgs(args = {Object.class})
public class Negation extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "not";
  }

  @Override
  public SCMBoolean apply(Object... args) {
    return SCMBoolean.toSCMBoolean(!SCMBoolean.valueOf(args[0]));
  }
}
