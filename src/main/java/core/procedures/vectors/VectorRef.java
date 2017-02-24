package core.procedures.vectors;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMClass;
import core.scm.SCMMutableVector;

public final class VectorRef extends AFn {

  public VectorRef() {
    super(new FnArgsBuilder().minArgs(2).maxArgs(2)
                             .mandatoryArgsTypes(new Class[]{SCMMutableVector.class, SCMClass.ExactNonNegativeInteger.class}));
  }

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
      throw new IllegalArgumentException(String.format("%s: value out of range: %s", getName(), pos));
    }
    return vec.get(pos.intValue());
  }
}
