package core.procedures.vectors;

import core.procedures.AFn;
import core.scm.SCMMutableVector;

public final class Vector extends AFn {

  @Override
  public String getName() {
    return "vector";
  }

  @Override
  public SCMMutableVector apply(Object... args) {
    return new SCMMutableVector(args);
  }
}
