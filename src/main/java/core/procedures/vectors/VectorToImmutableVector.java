package core.procedures.vectors;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMImmutableVector;
import core.scm.SCMVector;

public final class VectorToImmutableVector extends AFn {

  public VectorToImmutableVector() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{SCMVector.class}).build());
  }

  @Override
  public String getName() {
    return "vector->immutable-vector";
  }

  @Override
  public Object apply1(Object arg) {
    if (arg instanceof SCMImmutableVector) {
      return arg;
    } else {
      return new SCMImmutableVector(((SCMVector)arg).getArray());
    }
  }
}