package core.procedures.vectors;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMClass;
import core.scm.SCMMutableVector;

@FnArgs(minArgs = 2, maxArgs = 2, mandatoryArgsTypes = {SCMMutableVector.class, SCMClass.ExactNonNegativeInteger.class})
public class VectorRef extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "vector-ref";
  }

  @Override
  public Object apply2(Object arg1, Object arg2) {
    SCMMutableVector vec = (SCMMutableVector)arg1;
    Long pos = ((Number)arg2).longValue();
    if (pos >= vec.length()) {
      throw new IllegalArgumentException(String.format("Value out of range: %s", pos));
    }
    return vec.get(pos.intValue());
  }
}
