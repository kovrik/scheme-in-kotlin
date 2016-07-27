package core.procedures.cons;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMCons;

import java.util.List;

public class ListTail extends AFn {

  @Override
  public String getName() {
    return "list-tail";
  }

  @Override
  public Object invoke(Object... args) {
    if (args.length != 2) {
      throw new ArityException(args.length, 2, getName());
    }

    Object o = args[0];
    Object p = args[1];
    if (!(p instanceof Long)) {
      throw new WrongTypeException("Integer", p);
    }
    Long p1 = (Long) p;
    if (p1 == 0) {
      return o;
    }
    if (!(o instanceof List)) {
      throw new WrongTypeException("List", o);
    }
    List list = (List)o;
    if (p1 >= list.size() + 1) {
      throw new IllegalArgumentException("Value out of range: " + p1);
    }
    /* Cons cell */
    if ((list instanceof SCMCons) && !((SCMCons)list).isList()) {
      if (p1 == 1) {
        return ((SCMCons)list).cdr();
      } else {
        throw new IllegalArgumentException("Value out of range: " + p1);
      }
    }
    return list.subList(p1.intValue(), list.size());
  }
}
