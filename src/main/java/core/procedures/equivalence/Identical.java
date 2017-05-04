package core.procedures.equivalence;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

public class Identical extends AFn {

  public Identical() {
    super(new FnArgsBuilder().min(2).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "identical?";
  }

  @Override
  public Boolean apply(Object... args) {
    Boolean result = Boolean.TRUE;
    for (int i = 0; i < args.length - 1; i++) {
      result = result && apply2(args[i], args[i + 1]);
    }
    return result;
  }

  @Override
  public Boolean apply2(Object arg1, Object arg2) {
    return arg1 == arg2;
  }
}
