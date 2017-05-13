package core.procedures.generic;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

import java.util.Collection;
import java.util.Iterator;

public class Second extends AFn {

  public Second() {
    super(new FnArgsBuilder().min(1).max(1).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "second";
  }

  @Override
  public Object apply1(Object arg) {
    return second(arg);
  }

  public static Object second(Object arg) {
    if (arg instanceof Collection) {
      if (((Collection)arg).size() < 2) {
        return null;
      }
      Iterator iterator = ((Collection)arg).iterator();
      iterator.next();
      return iterator.next();
    } else if (arg instanceof CharSequence) {
      CharSequence cs = (CharSequence) arg;
      return cs.length() < 2 ? null : cs.charAt(1);
    }
    throw new WrongTypeException("second", "List or Vector or Set or String", arg);
  }
}
