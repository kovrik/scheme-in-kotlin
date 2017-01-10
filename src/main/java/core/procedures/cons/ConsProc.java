package core.procedures.cons;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMCons;

@FnArgs(minArgs = 2, maxArgs = 2)
public class ConsProc extends AFn {

  @Override
  public String getName() {
    return "cons";
  }

  @Override
  public SCMCons apply2(Object arg1, Object arg2) {
    return cons(arg1, arg2);
  }

  public static SCMCons cons(Object car, Object cdr) {
    if (car == null && cdr == null) {
      return SCMCons.NIL;
    }
    return SCMCons.cons(car, cdr);
  }
}
