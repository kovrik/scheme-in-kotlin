package core.procedures.vectors;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.Type;
import core.scm.MutableVector;

public final class VectorRef extends AFn {

  public VectorRef() {
    super(new FnArgsBuilder().min(2).max(2)
                             .mandatory(new Class[]{MutableVector.class, Type.ExactNonNegativeInteger.class}).build());
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
    MutableVector vec = (MutableVector)arg1;
    Long pos = ((Number)arg2).longValue();
    if (pos >= vec.size()) {
      throw new IndexOutOfBoundsException(String.format("%s: value out of range: %s", getName(), pos));
    }
    return vec.get(pos.intValue());
  }
}
