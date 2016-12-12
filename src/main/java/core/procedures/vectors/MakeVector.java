package core.procedures.vectors;

import core.exceptions.ArityException;
import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMClass;
import core.scm.SCMMutableVector;

import static core.scm.SCMUnspecified.UNSPECIFIED;

@FnArgs(isVariadic = true, args = {SCMClass.ExactNonNegativeInteger.class})
public class MakeVector extends AFn {

  @Override
  public String getName() {
    return "make-vector";
  }

  @Override
  public Object invoke(Object... args) {
    Long s = ((Number)args[0]).longValue();
    Object init = UNSPECIFIED;
    if (args.length == 2) {
      init = args[1];
    } else if (args.length > 2) {
      throw new ArityException(args.length, getName());
    }
    return new SCMMutableVector(s.intValue(), init);
  }
}
