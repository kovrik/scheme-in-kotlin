package core.procedures.generic;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.Vector;
import core.utils.Utils;

import java.util.*;

public class Next extends AFn {

  public Next() {
    super(new FnArgsBuilder().min(1).max(1).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "next";
  }

  @Override
  public Object apply1(Object arg) {
    if (!Utils.isSeqable(arg)) {
      throw new RuntimeException("Don't know how to create Sequence from " + arg.getClass());
    }
    if (arg instanceof List) {
      List list = (List) arg;
      return list.isEmpty() ? null : list.subList(1, list.size());
    } else if (arg instanceof Set) {
      Set set = (Set) arg;
      if (set.isEmpty()) {
        return null;
      }
      Set<Object> next = new HashSet<>();
      Iterator iter = set.iterator();
      iter.next();
      while (iter.hasNext()) {
        next.add(iter.next());
      }
      return next;
    } else if (arg instanceof Vector) {
      Vector vec = (Vector) arg;
      return vec.size() == 0 ? null : new Vector(Arrays.copyOfRange(vec.getArray(), 1, vec.size()));
    } else if (arg instanceof CharSequence) {
      CharSequence cs = (CharSequence) arg;
      return cs.length() == 0 ? null : cs.subSequence(1, cs.length());
    }
    throw new WrongTypeException("next", "List or Vector or Set or String", arg);
  }
}
