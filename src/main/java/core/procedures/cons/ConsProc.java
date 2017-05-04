package core.procedures.cons;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.Cons;

public final class ConsProc extends AFn {

  public ConsProc() {
    super(new FnArgsBuilder().min(2).max(2).build());
  }

  @Override
  public String getName() {
    return "cons";
  }

  @Override
  public Cons apply2(Object arg1, Object arg2) {
    return cons(arg1, arg2);
  }

  public static Cons cons(Object car, Object cdr) {
    if (car == null && cdr == null) {
      return Cons.EMPTY;
    }
    return Cons.cons(car, cdr);
  }
}
