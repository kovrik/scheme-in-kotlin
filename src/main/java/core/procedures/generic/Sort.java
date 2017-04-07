package core.procedures.generic;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMVector;

import java.util.*;

public final class Sort extends AFn {

  public Sort() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1));
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "sort";
  }

  // TODO accept comparator as optional first argument
  @Override
  public Object apply1(Object arg) {
    try {
      if (arg instanceof List) {
        Collections.sort((List) arg);
        return arg;
      }
      if (arg instanceof SCMVector) {
        Arrays.sort(((SCMVector) arg).getArray());
        return arg;
      }
      if (arg instanceof Map) {
        return new TreeMap<>((Map) arg);
      }
    } catch (ClassCastException e) {
      // ignore
    }
    throw new WrongTypeException(getName(), "Collection of comparable elements", arg);
  }
}
