package core.procedures.vectors;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMMutableVector;

@FnArgs(args = {SCMMutableVector.class})
public class VectorLength extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "vector-length";
  }

  @Override
  public Long invoke(Object... args) {
    return ((Integer)((SCMMutableVector)args[0]).length()).longValue();
  }
}
