package core.procedures.cons;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMClass;
import core.scm.SCMCons;

public final class Append extends AFn {

  public Append() {
    super(new FnArgsBuilder().rest(SCMClass.SCMProperList.class)
                             .last(Object.class).build());
  }

  @Override
  public String getName() {
    return "append";
  }

  @Override
  public Object apply(Object... args) {
    Object result = SCMCons.EMPTY;
    for (Object arg : args) {
      result = append(result, arg);
    }
    return result;
  }

  public static Object append(Object first, Object second) {
    if (SCMCons.isNull(first)) {
      return second;
    }
    return SCMCons.cons(Car.car(first), append(Cdr.cdr(first), second));
  }
}
