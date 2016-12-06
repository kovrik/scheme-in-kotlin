package core.procedures.vectors;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMVector;

@FnArgs(isVariadic = true)
public class Vector extends AFn {

  @Override
  public String getName() {
    return "vector";
  }

  @Override
  public SCMVector invoke(Object... args) {
    return new SCMVector(args);
  }
}
