package core.procedures.vectors;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMImmutableVector;
import core.scm.SCMVector;

@FnArgs(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = {SCMVector.class})
public class VectorToImmutableVector extends AFn {

  @Override
  public String getName() {
    return "vector->immutable-vector";
  }

  @Override
  public Object apply(Object... args) {
    if (args[0] instanceof SCMImmutableVector) {
      return args[0];
    } else {
      return new SCMImmutableVector(((SCMVector)args[0]).getArray());
    }
  }
}