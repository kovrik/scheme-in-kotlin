package core.procedures.equivalence;

import core.procedures.AFn;
import core.scm.SCMBoolean;
import core.scm.SCMSymbol;

import static core.scm.SCMCons.NIL;

public class Eq extends AFn {

  @Override
  public SCMBoolean invoke(Object... args) {
    Boolean result = Boolean.TRUE;
    if (args != null && args.length > 1) {
      for (int i = 0; i < args.length - 1; i++) {
        result = result && eq(args[i], args[i + 1]);
      }
    }
    return SCMBoolean.toSCMBoolean(result);
  }

  public static boolean eq(Object first, Object second) {
    if ((first instanceof SCMSymbol) && (second instanceof SCMSymbol)) {
      return first.equals(second);
    }
    return NIL.equals(first) && (NIL.equals(second)) || first == second;
  }
}
