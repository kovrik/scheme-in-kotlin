package core.procedures.cons;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMBoolean;
import core.scm.SCMCons;

public class Append extends AFn {

  @Override
  public Object invoke(Object... args) {
    if (args.length == 0) {
      return SCMCons.NIL;
    }
    if (args.length == 1) {
      return args[0];
    }
    Object result = args[0];
    if (!SCMCons.isList(result)) {
      throw new WrongTypeException("List", result);
    }

    for (int i = 1; i < args.length; i++) {
      Object current = args[i];
      /* Do not check last element */
      if ((i != args.length - 1) && !SCMCons.isList(current)) {
        throw new WrongTypeException("List", current);
      }
      result = append2(result, current);
    }
    return result;
  }

  // FIXME Make iterative!
  public static Object append2(Object first, Object second) {
    if (SCMBoolean.valueOf(IsNull.isNull(first))) {
      return second;
    }
    return SCMCons.cons(Car.car(first), append2(Cdr.cdr(first), second));
  }
}
