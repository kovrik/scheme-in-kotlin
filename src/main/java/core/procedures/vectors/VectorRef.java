package core.procedures.vectors;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMVector;

@FnArgs(args = {SCMVector.class, Long.class})
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
    SCMVector vec = (SCMVector)args[0];
    Object p = args[1];
    Long pos = (Long)p;
    if ((pos < 0) || (pos >= vec.length())) {
      throw new IllegalArgumentException(String.format("Value out of range: %s", pos));
    }
    return vec.get(pos.intValue());
  }
}
