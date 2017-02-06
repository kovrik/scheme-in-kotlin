package core.procedures.vectors;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMMutableVector;

import java.util.Arrays;

import static core.scm.SCMConstant.UNSPECIFIED;

@FnArgs(minArgs = 2, maxArgs = 2, mandatoryArgsTypes = {SCMMutableVector.class, Object.class})
public final class VectorFill extends AFn {

  @Override
  public String getName() {
    return "vector-fill!";
  }

  @Override
  public Object apply2(Object arg1, Object arg2) {
    SCMMutableVector vector = (SCMMutableVector) arg1;
    Arrays.fill(vector.getArray(), arg2);
    return UNSPECIFIED;
  }
}
