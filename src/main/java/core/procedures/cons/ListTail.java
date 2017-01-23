package core.procedures.cons;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMClass;
import core.scm.SCMCons;

import java.util.List;

@FnArgs(minArgs = 2, maxArgs = 2, mandatoryArgsTypes = {Object.class, SCMClass.ExactNonNegativeInteger.class})
public final class ListTail extends AFn {

  @Override
  public String getName() {
    return "list-tail";
  }

  @Override
  public Object apply2(Object arg1, Object arg2) {
    Long p = ((Number)arg2).longValue();
    if (p == 0) {
      return arg1;
    }
    if (!(arg1 instanceof List)) {
      throw new WrongTypeException("List", arg1);
    }
    List list = (List) arg1;
    if (p >= list.size() + 1) {
      throw new IllegalArgumentException(String.format("%s: value out of range: %s", getName(), p));
    }
    /* Cons cell */
    if ((list instanceof SCMCons) && !((SCMCons)list).isList()) {
      if (p == 1) {
        return ((SCMCons)list).cdr();
      } else {
        throw new IllegalArgumentException(String.format("%s: value out of range: %s", getName(), p));
      }
    }
    return list.subList(p.intValue(), list.size());
  }
}
