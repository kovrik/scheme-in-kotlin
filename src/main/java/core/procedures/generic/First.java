package core.procedures.generic;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

import java.util.Collection;

public class First extends AFn {

  public First() {
    super(new FnArgsBuilder().min(1).max(1).build());
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
    if (arg instanceof Collection) {
      return ((Collection)arg).isEmpty() ? null : ((Collection) arg).iterator().next();
    } else if (arg instanceof CharSequence) {
      CharSequence cs = (CharSequence) arg;
      return cs.length() == 0 ? null : cs.charAt(0);
    }
    throw new WrongTypeException("first", "List or Vector or Set or String", arg);
  }
}
