package core.procedures.vectors;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMImmutableVector;
import core.scm.SCMVector;

@FnArgs(args = {SCMVector.class})
public class VectorToImmutableVector extends AFn {

  @Override
  public String getName() {
    return "vector->immutable-vector";
  }

  @Override
  public Object invoke(Object... args) {
    if (args[0] instanceof SCMImmutableVector) {
      return args[0];
    } else {
      return new SCMImmutableVector(((SCMVector)args[0]).getArray());
    }
  }
}