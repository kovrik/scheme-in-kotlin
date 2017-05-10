package core.procedures.vectors;

import core.procedures.AFn;
import core.scm.Vector;

public final class VectorImmutable extends AFn {

  @Override
  public String getName() {
    return "vector-immutable";
  }

  @Override
  public Vector apply(Object... args) {
    return new Vector(args);
  }
}
