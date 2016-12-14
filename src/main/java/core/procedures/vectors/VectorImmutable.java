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
  public SCMImmutableVector apply(Object... args) {
    return new SCMImmutableVector(args);
  }
}
