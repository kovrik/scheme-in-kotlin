package core.procedures.equivalence;

import core.procedures.AFn;
import core.scm.SCMBoolean;
import core.scm.SCMCons;
import core.scm.SCMSymbol;

import java.util.List;

public class Eqv extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "eqv?";
  }

  @Override
  public SCMBoolean apply(Object... args) {
    Boolean result = Boolean.TRUE;
    if (args != null && args.length > 1) {
      for (int i = 0; i < args.length - 1; i++) {
        result = result && eqv(args[i], args[i + 1]);
      }
    }
    return SCMBoolean.toSCMBoolean(result);
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
}
