package core.procedures.vectors;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMVector;

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
    if (args != null && args.length == 1) {
      if (args[0] instanceof SCMVector) {
        return ((Integer)((SCMVector)args[0]).length()).longValue();
      }
      throw new WrongTypeException("Vector", args[0]);
    }
    throw new ArityException(args.length, 1, getName());
  }
}
