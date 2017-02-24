package core.procedures.vectors;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMImmutableVector;
import core.scm.SCMVector;

public final class VectorToImmutableVector extends AFn {

  public VectorToImmutableVector() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1).mandatoryArgsTypes(new Class[]{SCMVector.class}));
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