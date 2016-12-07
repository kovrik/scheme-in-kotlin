package core.procedures.cons;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMCons;

import java.util.List;

@FnArgs(args = {Object.class, Long.class})
public class ListTail extends AFn {

  @Override
  public String getName() {
    return "list-tail";
  }

  @Override
  public Object invoke(Object... args) {
    Object o = args[0];
    Long p = (Long) args[1];
    if (p == 0) {
      return o;
    }
    if (!(o instanceof List)) {
      throw new WrongTypeException("List", o);
    }
    List list = (List)o;
    if (p >= list.size() + 1) {
      throw new IllegalArgumentException("Value out of range: " + p);
    }
    /* Cons cell */
    if ((list instanceof SCMCons) && !((SCMCons)list).isList()) {
      if (p == 1) {
        return ((SCMCons)list).cdr();
      } else {
        throw new IllegalArgumentException("Value out of range: " + p);
      }
    }
    return list.subList(p.intValue(), list.size());
  }
}
