package core.procedures.vectors;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMVector;

import java.util.Arrays;

import static core.scm.SCMUnspecified.UNSPECIFIED;

public class VectorFill extends AFn {

  @Override
  public Object invoke(Object... args) {
    if (args != null && args.length == 2) {
      if (args[0] instanceof SCMVector) {
        SCMVector vector = (SCMVector) args[0];
        Arrays.fill(vector.getArray(), args[1]);
        return UNSPECIFIED;
      }
      throw new WrongTypeException("Vector", args[0]);
    }
    throw new ArityException(args.length, 2, "vector-fill!");
  }
}
