package core.procedures.vectors;

import core.exceptions.ArityException;
import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMVector;

import static core.scm.SCMUnspecified.UNSPECIFIED;

@FnArgs(isVariadic = true, args = {Long.class})
public class MakeVector extends AFn {

  @Override
  public String getName() {
    return "make-vector";
  }

  @Override
  public Object invoke(Object... args) {
    Long s = (Long)args[0];
    if (s < 0) {
      throw new IllegalArgumentException(String.format("Size value is out of range in `%s`", getName()));
    }
    Object init = UNSPECIFIED;
    if (args.length == 2) {
      init = args[1];
    } else if (args.length > 2) {
      throw new ArityException(args.length, getName());
    }
    return new SCMVector(s.intValue(), init);
  }
}
