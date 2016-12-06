package core.procedures.vectors;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMVector;

import static core.scm.SCMUnspecified.UNSPECIFIED;

@FnArgs(args = {SCMVector.class, Long.class, Object.class})
public class VectorSet extends AFn {

  @Override
  public String getName() {
    return "vector-set!";
  }

  @Override
  public Object invoke(Object... args) {
    SCMVector vec = (SCMVector)args[0];
    Object p = args[1];
    Long pos = (Long)p;
    if ((pos < 0) || (pos >= vec.length())) {
      throw new IllegalArgumentException(String.format("Value out of range: %s", pos));
    }
    vec.set(pos.intValue(), args[2]);
    return UNSPECIFIED;
  }
}
