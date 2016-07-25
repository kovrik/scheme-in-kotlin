package core.procedures.cons;

import core.exceptions.ArityException;
import core.procedures.AFn;
import core.scm.SCMCons;

public class ConsProc extends AFn {

  @Override
  public SCMCons invoke(Object... args) {
    if (args.length != 2) {
      throw new ArityException(args.length, 2, "cons");
    }
    return cons(args[0], args[1]);
  }

  public static SCMCons cons(Object car, Object cdr) {
    if (car == null && cdr == null) {
      return SCMCons.NIL;
    }
    return SCMCons.cons(car, cdr);
  }
}
