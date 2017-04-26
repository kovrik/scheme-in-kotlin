package core.procedures.generic;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMVector;

import java.util.List;
import java.util.Set;

public class First extends AFn {

  public First() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1));
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "first";
  }

  @Override
  public Object apply1(Object arg) {
    return first(arg);
  }

  public static Object first(Object arg) {
    if (arg instanceof List) {
      List list = (List) arg;
      return list.isEmpty() ? null : list.get(0);
    } else if (arg instanceof Set) {
      Set set = (Set) arg;
      return set.isEmpty() ? null : set.iterator().next();
    } else if (arg instanceof SCMVector) {
      SCMVector vec = (SCMVector) arg;
      return vec.length() == 0 ? null : vec.get(0);
    } else if (arg instanceof CharSequence) {
      CharSequence cs = (CharSequence) arg;
      return cs.length() == 0 ? null : cs.charAt(0);
    }
    throw new WrongTypeException("first", "List or Vector or Set or String", arg);
  }
}
