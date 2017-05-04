package core.procedures.vectors;

import core.procedures.AFn;
import core.scm.MutableVector;

public final class Vector extends AFn {

  @Override
  public String getName() {
    return "vector";
  }

  @Override
  public MutableVector apply(Object... args) {
    return new MutableVector(args);
  }
}
