package core.procedures.vectors;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMCons;
import core.scm.SCMVector;

public class VectorToList extends AFn {

  @Override
  public SCMCons invoke(Object... args) {
    if (args != null && args.length == 1) {
      if (args[0] instanceof SCMVector) {
        SCMVector vector = (SCMVector) args[0];
        return SCMCons.list(vector.getArray());
      }
      throw new WrongTypeException("Vector", args[0]);
    }
    throw new ArityException(args.length, 1, "vector->list");
  }

  public Number zero() {
    throw new ArityException(0, 1, "vector->list");
  }

  public Number apply(Number first, Number second) {
    throw new ArityException(2, 1, "vector->list");
  }

  public Object apply(Object first, Object second) {
    throw new ArityException(2, 1, "vector->list");
  }
}
