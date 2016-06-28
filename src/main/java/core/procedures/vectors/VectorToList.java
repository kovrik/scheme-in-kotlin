package core.procedures.vectors;

import core.exceptions.ArityException;
import core.procedures.AFn;
import core.scm.SCMList;
import core.scm.SCMVector;

public class VectorToList extends AFn {

  @Override
  public SCMList invoke(Object... args) {
    if (args != null && args.length == 1) {
      if (args[0] instanceof SCMVector) {
        SCMVector vector = (SCMVector) args[0];
        return new SCMList(vector.toArray());
      }
      throw new IllegalArgumentException("Wrong argument type. Expected: Vector, actual: " + args[0].getClass().getSimpleName());
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

  @Override
  public Object call() throws Exception {
    return invoke();
  }

  @Override
  public void run() {
    invoke();
  }
}
