package core.procedures.math;

import core.scm.SCMBoolean;
import core.exceptions.ArityException;
import core.procedures.AFn;

public class Negation extends AFn {

  @Override
  public String getName() {
    return "not";
  }

  @Override
  public SCMBoolean invoke(Object... args) {
    if (args != null && args.length == 1) {
      return SCMBoolean.toSCMBoolean(!SCMBoolean.valueOf(args[0]));
    }
    throw new ArityException(args.length, getName());
  }
}
