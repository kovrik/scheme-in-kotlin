package core.procedures.vectors;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.MutableVector;
import core.scm.Vector;

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
  public MutableVector apply1(Object arg) {
    if (arg instanceof Vector) {
      return new MutableVector(((Vector)arg).getArray());
    }
    if (arg instanceof List) {
      return new MutableVector(((List)arg).toArray());
    }
    if (arg instanceof CharSequence) {
      int size = ((CharSequence) arg).length();
      MutableVector vector = new MutableVector(size, null);
      for (int i = 0; i < size; i++) {
        vector.set(i, ((CharSequence)arg).charAt(i));
      }
      return vector;
    }
    throw new WrongTypeException(getName(), "List or Vector or String", arg);
  }
}
