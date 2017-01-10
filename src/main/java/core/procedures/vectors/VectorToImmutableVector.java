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
  public Object apply1(Object arg) {
    if (arg instanceof SCMImmutableVector) {
      return arg;
    } else {
      return new SCMImmutableVector(((SCMVector)arg).getArray());
    }
  }
}