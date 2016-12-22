package core.procedures.vectors;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMMutableVector;
import core.scm.SCMVector;

@FnArgs(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = {SCMVector.class})
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
  public Long apply(Object... args) {
    return ((Integer)((SCMMutableVector)args[0]).length()).longValue();
  }
}
