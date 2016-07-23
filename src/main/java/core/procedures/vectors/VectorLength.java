package core.procedures.vectors;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMVector;

public class VectorLength extends AFn {

  @Override
  public Long invoke(Object... args) {
    if (args != null && args.length == 1) {
      if (args[0] instanceof SCMVector) {
        return ((Integer)((SCMVector)args[0]).length()).longValue();
      }
      throw new WrongTypeException("Vector", args[0]);
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
}
