package core.procedures.vectors;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMClass;
import core.scm.SCMMutableVector;

import static core.scm.SCMUnspecified.UNSPECIFIED;

@FnArgs(minArgs = 1, maxArgs = 2, mandatoryArgsTypes = {SCMClass.ExactNonNegativeInteger.class})
public class MakeVector extends AFn {

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
