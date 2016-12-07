package core.procedures.cons;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.ICons;
import core.scm.SCMCons;

import java.util.List;

@FnArgs(args = {SCMCons.SCMPair.class})
public class Car extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

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
    return ((List)o).get(0);
  }
}
