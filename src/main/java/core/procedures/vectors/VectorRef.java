package core.procedures.vectors;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMVector;

public class VectorRef extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "vector-ref";
  }

  @Override
  public Object invoke(Object... args) {
    if (args.length != 2) {
      throw new ArityException(args.length, 2, getName());
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
    return vec.get(pos.intValue());
  }
}
