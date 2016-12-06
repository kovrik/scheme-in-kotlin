package core.procedures.vectors;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMVector;

@FnArgs(args = {SCMVector.class})
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
    return ((Integer)((SCMVector)args[0]).length()).longValue();
  }
}
