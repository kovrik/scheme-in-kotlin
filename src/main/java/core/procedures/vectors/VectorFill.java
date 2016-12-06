package core.procedures.vectors;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMVector;

import java.util.Arrays;

import static core.scm.SCMUnspecified.UNSPECIFIED;

@FnArgs(args = {SCMVector.class, Object.class})
public class VectorFill extends AFn {

  @Override
  public String getName() {
    return "vector-fill!";
  }

  @Override
  public Object invoke(Object... args) {
    SCMVector vector = (SCMVector) args[0];
    Arrays.fill(vector.getArray(), args[1]);
    return UNSPECIFIED;
  }
}
