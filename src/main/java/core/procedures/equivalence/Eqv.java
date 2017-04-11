package core.procedures.equivalence;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMCons;
import core.scm.SCMSymbol;

import java.util.List;

public final class Eqv extends AFn {

  public Eqv() {
    super(new FnArgsBuilder().minArgs(2));
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "eqv?";
  }

  @Override
  public Boolean apply(Object... args) {
    Boolean result = Boolean.TRUE;
    for (int i = 0; i < args.length - 1; i++) {
      result = result && eqv(args[i], args[i + 1]);
    }
    return result;
  }

  @Override
  public Boolean apply2(Object arg1, Object arg2) {
    return eqv(arg1, arg2);
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
