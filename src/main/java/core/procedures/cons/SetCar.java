package core.procedures.cons;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMCons;

import java.util.List;

import static core.scm.SCMUnspecified.UNSPECIFIED;

public class SetCar extends AFn {

  @Override
  public Object invoke(Object... args) {
    if (args.length != 2) {
      throw new ArityException(args.length, 2, "set-car!");

    }
    Object p = args[0];
    if (!SCMCons.isPair(p)) {
      throw new WrongTypeException("Pair", p);
    }
    List cons = (List)p;
    cons.set(0, args[1]);
    return UNSPECIFIED;
  }
}
