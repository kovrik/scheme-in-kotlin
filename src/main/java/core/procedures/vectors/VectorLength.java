package core.procedures.vectors;

import core.exceptions.ArityException;
import core.procedures.AFn;
import core.scm.SCMVector;

public class VectorLength extends AFn {

  @Override
  public Long invoke(Object... args) {
    if (args != null && args.length == 1) {
      if (args[0] instanceof SCMVector) {
        return ((Integer)((SCMVector)args[0]).length()).longValue();
      }
      throw new IllegalArgumentException("Wrong argument type. Expected: Vector, actual: " + args[0].getClass().getSimpleName());
    }
    throw new ArityException(args.length, 1, "vector-length");
  }

  public Number zero() {
    throw new ArityException(0, 1, "vector-length");
  }

  public Number apply(Number first, Number second) {
    throw new ArityException(2, 1, "vector-length");
  }

  public Object apply(Object first, Object second) {
    throw new ArityException(2, 1, "vector-length");
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
