package core.procedures.cons;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMCons;

public final class ConsProc extends AFn {

  public ConsProc() {
    super(new FnArgsBuilder().min(2).max(2).build());
  }

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
      return SCMCons.EMPTY;
    }
    return SCMCons.cons(car, cdr);
  }
}
