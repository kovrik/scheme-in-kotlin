package core.procedures.equivalence;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.MutableString;

public final class Equal extends AFn {

  public Equal() {
    super(new FnArgsBuilder().min(2).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "equal?";
  }

  @Override
  public Boolean apply(Object... args) {
    Boolean result = Boolean.TRUE;
    for (int i = 0; i < args.length - 1; i++) {
      result = result && equal(args[i], args[i + 1]);
    }
    return result;
  }

  @Override
  public Boolean apply2(Object arg1, Object arg2) {
    return equal(arg1, arg2);
  }

  private boolean equal(Object first, Object second) {
    if ((first instanceof CharSequence) && (second instanceof MutableString)) {
      return second.equals(first);
    }
    return first.equals(second);
  }
}
