package core.procedures.cons;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.Cons;
import core.scm.Type;

public final class Append extends AFn {

  public Append() {
    super(new FnArgsBuilder().rest(Type.ProperList.class).last(Object.class).build());
  }

  @Override
  public String getName() {
    return "append";
  }

  @Override
  public Object apply(Object... args) {
    Object result = Cons.EMPTY;
    for (Object arg : args) {
      result = append(result, arg);
    }
    return result;
  }

  public static Object append(Object first, Object second) {
    if (Cons.isNull(first)) {
      return second;
    }
    return Cons.cons(Car.car(first), append(Cdr.cdr(first), second));
  }
}
