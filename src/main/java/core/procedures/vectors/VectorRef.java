package core.procedures.vectors;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMMutableVector;

@FnArgs(args = {SCMMutableVector.class, Long.class})
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
  public Object invoke(Object... args) {
    SCMMutableVector vec = (SCMMutableVector)args[0];
    Long pos = ((Number)args[1]).longValue();
    if ((pos < 0) || (pos >= vec.length())) {
      throw new IllegalArgumentException(String.format("Value out of range: %s", pos));
    }
    return vec.get(pos.intValue());
  }
}
