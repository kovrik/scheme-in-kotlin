package core.procedures.vectors;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMClass;
import core.scm.SCMMutableVector;

import static core.scm.SCMConstant.UNSPECIFIED;

public final class MakeVector extends AFn {

  public MakeVector() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(2).mandatoryArgsTypes(new Class[]{SCMClass.ExactNonNegativeInteger.class}));
  }

  @Override
  public String getName() {
    return "make-vector";
  }

  @Override
  public Object apply(Object... args) {
    Long s = ((Number)args[0]).longValue();
    Object init = UNSPECIFIED;
    if (args.length == 2) {
      init = args[1];
    }
    return new SCMMutableVector(s.intValue(), init);
  }
}
