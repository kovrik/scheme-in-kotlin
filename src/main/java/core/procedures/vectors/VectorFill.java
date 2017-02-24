package core.procedures.vectors;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMMutableVector;

import java.util.Arrays;

import static core.scm.SCMConstant.UNSPECIFIED;

public final class VectorFill extends AFn {

  public VectorFill() {
    super(new FnArgsBuilder().minArgs(2).maxArgs(2).mandatoryArgsTypes(new Class[]{SCMMutableVector.class, Object.class}));
  }

  @Override
  public String getName() {
    return "vector-fill!";
  }

  @Override
  public Object apply2(Object arg1, Object arg2) {
    SCMMutableVector vector = (SCMMutableVector) arg1;
    Arrays.fill(vector.getArray(), arg2);
    return UNSPECIFIED;
  }
}
