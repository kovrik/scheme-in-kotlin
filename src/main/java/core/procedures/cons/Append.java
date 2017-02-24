package core.procedures.cons;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMClass;
import core.scm.SCMCons;

import java.util.Arrays;

public final class Append extends AFn {

  public Append() {
    super(new FnArgsBuilder().restArgsType(new Class[]{SCMClass.SCMProperList.class})
                             .lastArgType(new Class[]{Object.class}));
  }

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
