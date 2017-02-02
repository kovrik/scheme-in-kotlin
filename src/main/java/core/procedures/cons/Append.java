package core.procedures.cons;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMClass;
import core.scm.SCMCons;

import java.util.Arrays;

@FnArgs(restArgsType = SCMClass.SCMProperList.class, lastArgType = Object.class)
public final class Append extends AFn {

  @Override
  public String getName() {
    return "append";
  }

  @Override
  public Object apply(Object... args) {
    return Arrays.stream(args).reduce(SCMCons.NIL, Append::append);
  }

  public static Object append(Object first, Object second) {
    if (SCMCons.isNull(first)) {
      return second;
    }
    return SCMCons.cons(Car.car(first), append(Cdr.cdr(first), second));
  }
}
