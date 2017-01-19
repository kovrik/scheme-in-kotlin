package core.procedures.cons;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMClass;
import core.scm.SCMCons;

@FnArgs(restArgsType = SCMClass.SCMProperList.class, lastArgType = Object.class)
public final class Append extends AFn {

  @Override
  public String getName() {
    return "append";
  }

  @Override
  public Object apply(Object... args) {
    if (args.length == 0) {
      return SCMCons.NIL;
    }
    if (args.length == 1) {
      return args[0];
    }
    Object result = args[0];
    for (int i = 1; i < args.length; i++) {
      Object current = args[i];
      result = append(result, current);
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
