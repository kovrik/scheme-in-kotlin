package core.procedures.cons;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMCons;
import core.scm.SCMMutableVector;
import core.scm.SCMVector;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

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
    if (arg instanceof SCMVector) {
      Object[] array = ((SCMVector) arg).getArray();
      SCMMutableVector reversed = new SCMMutableVector(Arrays.copyOf(array, array.length));
      Collections.reverse(Arrays.asList(reversed.getArray()));
      return reversed;
    }
    throw new WrongTypeException(getName(), "List or Vector", arg);
  }
}
