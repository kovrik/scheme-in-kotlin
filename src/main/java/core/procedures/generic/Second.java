package core.procedures.generic;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.Vector;

import java.util.Iterator;
import java.util.List;
import java.util.Set;

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
    if (arg instanceof List) {
      List list = (List) arg;
      return list.size() < 2 ? null : list.get(1);
    } else if (arg instanceof Set) {
      Set set = (Set) arg;
      Iterator iterator = set.iterator();
      iterator.next();
      return set.size() < 2 ? null : iterator.next();
    } else if (arg instanceof Vector) {
      Vector vec = (Vector) arg;
      return vec.size() < 2 ? null : vec.get(1);
    } else if (arg instanceof CharSequence) {
      CharSequence cs = (CharSequence) arg;
      return cs.length() < 2 ? null : cs.charAt(1);
    }
    throw new WrongTypeException("second", "List or Vector or Set or String", arg);
  }
}
