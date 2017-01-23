package core.procedures.vectors;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMClass;
import core.scm.SCMMutableVector;

import static core.scm.SCMUnspecified.UNSPECIFIED;

@FnArgs(minArgs = 3, maxArgs = 3, mandatoryArgsTypes = {SCMMutableVector.class, SCMClass.ExactNonNegativeInteger.class})
public final class VectorSet extends AFn {

  @Override
  public String getName() {
    return "vector-set!";
  }

  @Override
  public Object apply3(Object arg1, Object arg2, Object arg3) {
    SCMMutableVector vec = (SCMMutableVector)arg1;
    Long pos = ((Number)arg2).longValue();
    if (pos >= vec.length()) {
      throw new IllegalArgumentException(String.format("%s: value out of range: %s", getName(), pos));
    }
    vec.set(pos.intValue(), arg3);
    return UNSPECIFIED;
  }
}
