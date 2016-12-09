package core.procedures.vectors;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMImmutableVector;

@FnArgs(isVariadic = true)
public class VectorImmutable extends AFn {

  @Override
  public String getName() {
    return "vector-immutable";
  }

  @Override
  public SCMImmutableVector invoke(Object... args) {
    return new SCMImmutableVector(args);
  }
}
