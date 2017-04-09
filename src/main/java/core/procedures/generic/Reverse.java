package core.procedures.generic;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.*;

import java.util.*;

public final class Reverse extends AFn {

  public Reverse() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1));
  }

  @Override
  public String getName() {
    return "reverse";
  }

  @Override
  public Object apply1(Object arg) {
    if (arg instanceof List) {
      SCMCons<Object> result = SCMCons.list();
      ((List) arg).forEach(result::push);
      return result;
    }
    if (arg instanceof Set) {
      return SCMCons.list((Set)arg);
    }
    if (arg instanceof SCMVector) {
      Object[] array = ((SCMVector) arg).getArray();
      SCMMutableVector reversed = new SCMMutableVector(Arrays.copyOf(array, array.length));
      Collections.reverse(Arrays.asList(reversed.getArray()));
      return reversed;
    }
    if (arg instanceof CharSequence) {
      return new StringBuilder((CharSequence) arg).reverse().toString();
    }
    throw new WrongTypeException(getName(), "List or Vector or Set or String", arg);
  }
}
