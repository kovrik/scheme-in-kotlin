package main.core.procedures.equivalence;

import main.core.ast.SCMBoolean;
import main.core.procedures.AFn;
import main.core.procedures.math.IOperation;

public class Eqv extends AFn implements IOperation {

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

    if (first instanceof Character && second instanceof Character) {
      return ((Character) first).equals((Character)second);
    } else if (first instanceof Number && second instanceof Number) {
      return ((Number) first).equals((Number)second);
    }
    return first == second;
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
