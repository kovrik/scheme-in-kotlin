package core.procedures.equivalence;

import core.procedures.AFn;
import core.scm.SCMSymbol;

import static core.scm.SCMCons.NIL;

public final class Eq extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "eq?";
  }

  @Override
  public Boolean apply(Object... args) {
    Boolean result = Boolean.TRUE;
    if (args.length > 1) {
      for (int i = 0; i < args.length - 1; i++) {
        result = result && eq(args[i], args[i + 1]);
      }
    }
    return result;
  }

  @Override
  public Boolean apply2(Object arg1, Object arg2) {
    return eq(arg1, arg2);
  }

  private boolean eq(Object first, Object second) {
    if ((first instanceof SCMSymbol) && (second instanceof SCMSymbol)) {
      return first.equals(second);
    }
    return NIL.equals(first) && (NIL.equals(second)) || first == second;
  }
}
