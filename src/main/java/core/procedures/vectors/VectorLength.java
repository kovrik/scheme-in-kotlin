package core.procedures.vectors;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMMutableVector;
import core.scm.SCMVector;

public final class VectorLength extends AFn {

  public VectorLength() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1).mandatoryArgsTypes(new Class[]{SCMVector.class}));
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "vector-length";
  }

  @Override
  public Long apply1(Object arg) {
    return ((Integer)((SCMMutableVector)arg).length()).longValue();
  }
}
