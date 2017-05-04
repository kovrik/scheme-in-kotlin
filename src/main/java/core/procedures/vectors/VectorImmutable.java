package core.procedures.vectors;

import core.procedures.AFn;
import core.scm.ImmutableVector;

public final class VectorImmutable extends AFn {

  @Override
  public String getName() {
    return "vector-immutable";
  }

  @Override
  public ImmutableVector apply1(Object arg) {
    return new ImmutableVector(arg);
  }
}
