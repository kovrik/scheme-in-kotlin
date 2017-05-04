package core.procedures.vectors;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMMutableVector;
import core.scm.SCMVoid;

import java.util.Arrays;

public final class VectorFill extends AFn {

  public VectorFill() {
    super(new FnArgsBuilder().min(2).max(2).mandatory(new Class[]{SCMMutableVector.class, Object.class}).build());
  }

  @Override
  public String getName() {
    return "vector-fill!";
  }

  @Override
  public Object apply2(Object arg1, Object arg2) {
    SCMMutableVector vector = (SCMMutableVector) arg1;
    Arrays.fill(vector.getArray(), arg2);
    return SCMVoid.VOID;
  }
}
