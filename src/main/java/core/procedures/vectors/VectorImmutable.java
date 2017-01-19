package core.procedures.vectors;

import core.procedures.AFn;
import core.scm.SCMImmutableVector;

public final class VectorImmutable extends AFn {

  @Override
  public String getName() {
    return "vector-immutable";
  }

  @Override
  public SCMImmutableVector apply1(Object arg) {
    return new SCMImmutableVector(arg);
  }
}
