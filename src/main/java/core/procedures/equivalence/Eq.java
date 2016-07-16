package core.procedures.equivalence;

import core.procedures.AFn;
import core.procedures.math.IOperation;
import core.scm.SCMBoolean;
import core.scm.SCMSymbol;

import static core.scm.SCMCons.NIL;

public class Eq extends AFn implements IOperation {

  @Override
  public SCMBoolean invoke(Object... args) {
    Boolean result = zero();
    if (args != null && args.length > 1) {
      for (int i = 0; i < args.length - 1; i++) {
        result = result && apply(args[i], args[i + 1]);
      }
    }
    return SCMBoolean.toSCMBoolean(result);
  }

  public Boolean zero() {
    return Boolean.TRUE;
  }

  public Boolean apply(Object first, Object second) {
    return eq(first, second);
  }

  public static boolean eq(Object first, Object second) {
    if ((first instanceof SCMSymbol) && (second instanceof SCMSymbol)) {
      return first.equals(second);
    }
    return NIL.equals(first) && (NIL.equals(second)) || first == second;
  }

  @Override
  public Object call() throws Exception {
    return invoke();
  }

  @Override
  public void run() {
    invoke();
  }
}
