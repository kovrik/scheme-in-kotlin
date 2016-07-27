package core.procedures.vectors;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMVector;

import static core.scm.SCMUnspecified.UNSPECIFIED;

public class MakeVector extends AFn {

  @Override
  public String getName() {
    return "make-vector";
  }

  @Override
  public Object invoke(Object... args) {
    if (args.length < 1) {
      throw new ArityException(args.length, getName());
    }
    Object o = args[0];
    if (!(o instanceof Long)) {
      throw new WrongTypeException("Integer", o);
    }
    Long s = (Long)o;
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
