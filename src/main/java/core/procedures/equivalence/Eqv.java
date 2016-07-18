package core.procedures.equivalence;

import core.procedures.AFn;
import core.procedures.math.IOperation;
import core.scm.SCMBoolean;
import core.scm.SCMCons;
import core.scm.SCMSymbol;

import java.util.List;

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

  @Override
  public Object invoke(Object arg1, Object arg2) {
    return SCMBoolean.toSCMBoolean(eqv(arg1, arg2));
  }

  public Boolean zero() {
    return Boolean.TRUE;
  }

  public Boolean apply(Object first, Object second) {
    return eqv(first, second);
  }

  public static boolean eqv(Object first, Object second) {
    if (first instanceof Character && second instanceof Character) {
      return first.equals(second);
    } else if (first instanceof Number && second instanceof Number) {
      return first.equals(second);
    } else if (first instanceof SCMCons && second instanceof SCMCons) {
      return first == second;
    } else if (first instanceof List && second instanceof List) {
      return first.equals(second);
    } else if (first instanceof SCMSymbol && second instanceof SCMSymbol) {
      return first.equals(second);
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
