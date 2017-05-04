package core.procedures.vectors;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.ImmutableVector;
import core.scm.Vector;

public final class VectorToImmutableVector extends AFn {

  public VectorToImmutableVector() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{Vector.class}).build());
  }

  @Override
  public String getName() {
    return "vector->immutable-vector";
  }

  @Override
  public Object apply1(Object arg) {
    if (arg instanceof ImmutableVector) {
      return arg;
    } else {
      return new ImmutableVector(((Vector)arg).getArray());
    }
  }
}