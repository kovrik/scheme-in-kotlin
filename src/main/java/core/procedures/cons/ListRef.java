package core.procedures.cons;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMCons;

import java.util.List;

public class ListRef extends AFn {

  @Override
  public String getName() {
    return "list-ref";
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
    if (!(o instanceof List)) {
      throw new WrongTypeException("Pair", o);
    }
    List list = (List)o;
    if (p1 >= list.size()) {
      throw new IllegalArgumentException("Value out of range: " + p1);
    }
    /* Cons cell */
    if ((list instanceof SCMCons) && !((SCMCons)list).isList()) {
      if (p1 == 0) {
        return ((SCMCons)list).car();
      } else {
        throw new WrongTypeException("Pair", o);
      }
    }
    return list.get(p1.intValue());
  }
}
