package core.procedures.predicates;

import core.exceptions.ArityException;
import core.procedures.AFn;
import core.scm.SCMBoolean;
import core.utils.NumberUtils;

public class IsRational extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "rational?";
  }

  @Override
  public SCMBoolean invoke(Object... args) {
    if (args.length != 1) {
      throw new ArityException(args.length, 1, getName());
    }
    return SCMBoolean.toSCMBoolean(NumberUtils.isRational(args[0]));
  }
}
