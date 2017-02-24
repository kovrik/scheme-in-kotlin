package core.procedures.vectors;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMClass;
import core.scm.SCMMutableVector;

import static core.scm.SCMConstant.UNSPECIFIED;

public final class VectorSet extends AFn {

  public VectorSet() {
    super(new FnArgsBuilder().minArgs(3).maxArgs(3)
                             .mandatoryArgsTypes(new Class[]{SCMMutableVector.class, SCMClass.ExactNonNegativeInteger.class}));
  }

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
