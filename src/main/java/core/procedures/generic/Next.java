package core.procedures.generic;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMImmutableVector;
import core.scm.SCMVector;

import java.util.*;

public class Next extends AFn {

  public Next() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1));
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
    } else if (arg instanceof SCMVector) {
      SCMVector vec = (SCMVector) arg;
      return vec.length() == 0 ? null : new SCMImmutableVector(Arrays.copyOfRange(vec.getArray(), 1, vec.length()));
    } else if (arg instanceof CharSequence) {
      CharSequence cs = (CharSequence) arg;
      return cs.length() == 0 ? null : cs.subSequence(1, cs.length());
    }
    throw new WrongTypeException("next", "List or Vector or Set or String", arg);
  }
}
