package core.procedures.vectors;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.Type;
import core.scm.MutableVector;

public final class MakeVector extends AFn {

  public MakeVector() {
    super(new FnArgsBuilder().min(1).max(2).mandatory(new Class[]{Type.ExactNonNegativeInteger.class}).build());
  }

  @Override
  public String getName() {
    return "make-vector";
  }

  @Override
  public Object apply(Object... args) {
    Long s = ((Number)args[0]).longValue();
    Object init = null;
    if (args.length == 2) {
      init = args[1];
    }
    return new MutableVector(s.intValue(), init);
  }
}
