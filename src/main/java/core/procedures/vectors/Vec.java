package core.procedures.vectors;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMMutableVector;
import core.scm.SCMVector;

import java.util.List;

public final class Vec extends AFn {

  public Vec() {
    super(new FnArgsBuilder().min(1).max(1).build());
  }

  @Override
  public String getName() {
    return "vec";
  }

  @Override
  public SCMMutableVector apply1(Object arg) {
    if (arg instanceof SCMVector) {
      return new SCMMutableVector(((SCMVector)arg).getArray());
    }
    if (arg instanceof List) {
      return new SCMMutableVector(((List)arg).toArray());
    }
    if (arg instanceof CharSequence) {
      int size = ((CharSequence) arg).length();
      SCMMutableVector vector = new SCMMutableVector(size, null);
      for (int i = 0; i < size; i++) {
        vector.set(i, ((CharSequence)arg).charAt(i));
      }
      return vector;
    }
    throw new WrongTypeException(getName(), "List or Vector or String", arg);
  }
}
