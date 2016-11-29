package core.procedures.predicates;

import core.exceptions.ArityException;
import core.procedures.AFn;
import core.scm.SCMBoolean;
import core.scm.SCMOutputPort;

public class IsOutputPort extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "output-port?";
  }

  @Override
  public SCMBoolean invoke(Object... args) {
    if (args.length != 1) {
      throw new ArityException(args.length, 1, getName());
    }
    return SCMBoolean.toSCMBoolean(args[0] instanceof SCMOutputPort);
  }
}