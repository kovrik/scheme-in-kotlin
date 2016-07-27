package core.procedures.vectors;

import core.procedures.AFn;
import core.scm.SCMVector;

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
