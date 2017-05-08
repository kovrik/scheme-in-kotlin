package core.procedures.vectors;

import core.procedures.AFn;
import core.scm.Vector;

public final class VectorImmutable extends AFn {

  @Override
  public String getName() {
    return "vector-immutable";
  }

  @Override
  public Vector apply1(Object arg) {
    return new Vector(arg);
  }
}
