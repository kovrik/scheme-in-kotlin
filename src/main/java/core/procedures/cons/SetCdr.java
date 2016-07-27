package core.procedures.cons;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMCons;

import java.util.List;

import static core.scm.SCMUnspecified.UNSPECIFIED;

public class SetCdr extends AFn {

  @Override
  public Object invoke(Object... args) {
    if (args.length != 2) {
      throw new ArityException(args.length, 2, "set-cdr!");
    }
    Object p = args[0];
    if (!SCMCons.isPair(p)) {
      throw new WrongTypeException("Pair", p);
    }

    List list = (List)p;
    /* Remove tail */
    list.subList(1, list.size()).clear();

    /* Set new tail */
    Object o = args[1];
    if (o instanceof List) {
      list.addAll((List)o);
    } else {
      list.add(o);
      // FIXME How to make it Cons if it is a SubList?
      if (list instanceof SCMCons) {
        ((SCMCons)list).setList(false);
      }
    }
    return UNSPECIFIED;
  }
}