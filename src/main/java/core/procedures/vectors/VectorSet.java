package core.procedures.vectors;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMMutableVector;

import static core.scm.SCMUnspecified.UNSPECIFIED;

@FnArgs(args = {SCMMutableVector.class, Long.class, Object.class})
public class VectorSet extends AFn {

  @Override
  public String getName() {
    return "vector-set!";
  }

  @Override
  public Object invoke(Object... args) {
    SCMMutableVector vec = (SCMMutableVector)args[0];
    Long pos = ((Number)args[1]).longValue();
    if ((pos < 0) || (pos >= vec.length())) {
      throw new IllegalArgumentException(String.format("Value out of range: %s", pos));
    }
    vec.set(pos.intValue(), args[2]);
    return UNSPECIFIED;
  }
}
