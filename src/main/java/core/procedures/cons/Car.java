package core.procedures.cons;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.ICons;

import java.util.List;

public class Car extends AFn {

  @Override
  public String getName() {
    return "car";
  }

  @Override
  public Object invoke(Object... args) {
    if (args.length != 1) {
      throw new ArityException(args.length, 1, getName());
    }
    return car(args[0]);
  }

  public static Object car(Object o) {
    if (o instanceof ICons) {
      return ((ICons)o).car();
    }
    if (o instanceof List) {
      List list = (List) o;
      if (list.isEmpty()) {
        throw new WrongTypeException("Pair", list);
      }
      return list.get(0);
    }
    throw new WrongTypeException("Pair", o);
  }
}
