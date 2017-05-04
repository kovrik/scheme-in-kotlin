package core.procedures.vectors;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMMutableVector;
import core.scm.SCMVector;

public final class VectorLength extends AFn {

  public VectorLength() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{SCMVector.class}).build());
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
