package core.procedures.vectors;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.Type;
import core.scm.MutableVector;
import core.scm.Void;

public final class VectorSet extends AFn {

  public VectorSet() {
    super(new FnArgsBuilder().min(3).max(3)
                             .mandatory(new Class[]{MutableVector.class, Type.ExactNonNegativeInteger.class}).build());
  }

  @Override
  public String getName() {
    return "vector-set!";
  }

  @Override
  public Object apply3(Object arg1, Object arg2, Object arg3) {
    MutableVector vec = (MutableVector)arg1;
    Long pos = ((Number)arg2).longValue();
    if (pos >= vec.length()) {
      throw new IndexOutOfBoundsException(String.format("%s: value out of range: %s", getName(), pos));
    }
    vec.set(pos.intValue(), arg3);
    return Void.VOID;
  }
}
