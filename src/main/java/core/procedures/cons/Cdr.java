package core.procedures.cons;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.ICons;

import java.util.List;

public class Cdr extends AFn {

  @Override
  public String getName() {
    return "cdr";
  }

  @Override
  public Object invoke(Object... args) {
    if (args.length != 1) {
      throw new ArityException(args.length, 1, getName());
    }
    return cdr(args[0]);
  }

  public static Object cdr(Object o) {
    if (o instanceof ICons) {
      return ((ICons)o).cdr();
    }
    if (o instanceof List) {
      List list = (List) o;
      if (list.isEmpty()) {
        throw new WrongTypeException("Pair", list);
      }
      return list.subList(1, list.size());
    }
    throw new WrongTypeException("Pair", o);
  }
}
