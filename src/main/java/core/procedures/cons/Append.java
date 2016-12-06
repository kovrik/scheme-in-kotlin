package core.procedures.cons;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBoolean;
import core.scm.SCMCons;

@FnArgs(isVariadic = true)
public class Append extends AFn {

  @Override
  public String getName() {
    return "append";
  }

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
      result = append(result, current);
    }
    return result;
  }

  public static Object append(Object first, Object second) {
    if (SCMBoolean.valueOf(SCMCons.isNull(first))) {
      return second;
    }
    return SCMCons.cons(Car.car(first), append(Cdr.cdr(first), second));
  }
}
