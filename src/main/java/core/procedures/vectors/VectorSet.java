package core.procedures.vectors;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMVector;

import static core.scm.SCMUnspecified.UNSPECIFIED;

public class VectorSet extends AFn {

  @Override
  public Object invoke(Object... args) {
    if (args.length != 3) {
      throw new ArityException(args.length, 3, "vector-set!");
    }
    Object o = args[0];
    if (!(o instanceof SCMVector)) {
      throw new WrongTypeException("Vector", o);
    }
    SCMVector vec = (SCMVector)o;

    Object p = args[1];
    if (!(p instanceof Long)) {
      throw new WrongTypeException("Integer", p);
    }
    Long pos = (Long)p;
    if ((pos < 0) || (pos >= vec.length())) {
      throw new IllegalArgumentException(String.format("Value out of range: %s", pos));
    }
    vec.set(pos.intValue(), args[2]);
    return UNSPECIFIED;
  }
}
