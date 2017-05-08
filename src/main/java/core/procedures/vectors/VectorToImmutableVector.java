package core.procedures.vectors;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.MutableVector;
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
  public Vector apply1(Object arg) {
    if (arg.getClass() == MutableVector.class) {
      return new Vector(((Vector)arg).getArray());
    }
    return (Vector) arg;
  }
}